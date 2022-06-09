{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Resolver (resolve, ReferenceError) where

import Control.Monad (forM_, when, (>=>))
import Control.Monad.Extra (findM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Control.Monad.Trans.Except as Ex
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, local)
import Control.Monad.Trans.Writer.Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Lazy as W
import Data.Functor.Foldable (cataA, transverse)
import qualified Data.HashTable.IO as H
import Data.List (uncons)
import Data.Maybe (isJust)
import qualified Lib.Ast as Ast
import Lib.Interpreter.Error (RuntimeError (ReferenceError))

data ReferenceError = EmptyScopeStack | LocalInitializer | DuplicateVarName String | SelfInherit

instance Show ReferenceError where
    show EmptyScopeStack = "Tried to pop an empty scope stack."
    show LocalInitializer = "Cannot read local variable in its own initializer."
    show (DuplicateVarName name) = "A variable with the name `" ++ name ++ "` already exists in this scope"
    show SelfInherit = "A class can't inherit from itself."

type HashTable k v = H.BasicHashTable k v

type Initialized = Bool

type ScopeStack = [HashTable String Initialized]

type Solver = ReaderT ScopeStack (ExceptT ReferenceError IO)

throwE :: ReferenceError -> Solver a
throwE = lift . Ex.throwE

resolve :: [Ast.Stmt] -> ExceptT ReferenceError IO [Ast.Stmt]
resolve stmts = runReaderT (sequence (resolveStmt <$> stmts)) []

resolveStmt :: Ast.Stmt -> Solver Ast.Stmt
resolveStmt = cataA resolve
  where
    resolve :: Ast.StmtF (Solver Ast.Stmt) -> Solver Ast.Stmt
    resolve (Ast.BlockStmtF stmts) = do
        hm <- liftIO H.new
        stmts <- local (hm :) (sequence stmts)
        pure (Ast.BlockStmt stmts)
    resolve (Ast.VarStmtF name init) = do
        declare name
        newInit <- sequence $ resolveExpr <$> init
        define name
        pure $ Ast.VarStmt name newInit
    resolve (Ast.FunStmtF f) = Ast.FunStmt <$> resolveFunc f Ast.FunKind
    resolve (Ast.ExprStmtF expr) = Ast.ExprStmt <$> resolveExpr expr
    resolve (Ast.IfStmtF condition ifBranch elseBranch) = do
        newCond <- resolveExpr condition

        ifHm <- liftIO H.new
        newIf <- local (ifHm :) (sequence ifBranch)

        elseHm <- liftIO H.new
        newElse <- local (elseHm :) (sequence $ sequence <$> elseBranch)

        pure $ Ast.IfStmt newCond newIf newElse
    resolve (Ast.PrintStmtF expr) = Ast.PrintStmt <$> resolveExpr expr
    resolve (Ast.ReturnStmtF expr) = Ast.ReturnStmt <$> resolveExpr expr
    resolve (Ast.LoopStmtF stmts) = do
        hm <- liftIO H.new
        loop <- local (hm :) (sequence stmts)
        pure $ Ast.LoopStmt loop
    resolve Ast.BreakStmtF = pure Ast.BreakStmt
    resolve Ast.ContinueStmtF = pure Ast.ContinueStmt
    resolve
        Ast.ClassStmtF
            { Ast.classNameF = name,
              Ast.classMethodsF = cMethods,
              Ast.classSuperF = super
            } = do
            (declare >> define) name

            case super of
                Just (Ast.Variable {Ast.variableName}) | variableName == name -> throwE SelfInherit
                _ -> pure ()

            newSuper <- mapM resolveVar super

            localFun <-
                if isJust super
                    then do
                        superHm <- liftIO $ H.fromList [("super", True)]
                        pure (superHm :)
                    else pure id

            local localFun $ do
                hm <- liftIO $ H.fromList [("this", True)]
                methods <- local (hm :) (mapM (`resolveFunc` Ast.MethodKind) cMethods)
                pure $ Ast.ClassStmt {Ast.className = name, classMethods = methods, classSuper = super}

resolveFunc :: Ast.Function (Solver Ast.Stmt) -> Ast.FunType -> Solver (Ast.Function Ast.Stmt)
resolveFunc f@Ast.Function {..} typ = do
    case funcName of
        Just name -> (declare >> define) name
        Nothing -> pure ()
    hm <- liftIO H.new
    local (hm :) $ do
        forM_ funcParams (declare >> define)
        stmts <- sequence funcBody
        pure $ f {Ast.funcBody = stmts}

resolveVar :: Ast.Variable -> Solver Ast.Variable
resolveVar var@Ast.Variable {Ast.variableName} = do
    scopes <- ask
    case scopes of
        (x : _) -> do
            isInitialized <- liftIO $ H.lookup x variableName
            when
                (isInitialized == Just False)
                (throwE LocalInitializer)
        _ -> pure ()
    -- liftIO $ print ("Resolving `" ++ variableName ++ "` with scopes " ++ show scopes)
    level <- resolveLocal variableName
    pure $ var {Ast.variableLevel = level}

resolveExpr :: Ast.Expr -> Solver Ast.Expr
resolveExpr = cataA resolve
  where
    resolve :: Ast.ExprF (Solver Ast.Expr) -> Solver Ast.Expr
    resolve (Ast.VarF var) = Ast.Var <$> resolveVar var
    resolve (Ast.AssignF var@Ast.Variable {Ast.variableName} value) = do
        newVal <- value
        level <- resolveLocal variableName
        pure $ Ast.Assign var {Ast.variableLevel = level} newVal
    resolve (Ast.BinaryF op left right) = do
        newL <- left
        Ast.Binary op newL <$> right
    resolve (Ast.CallF callee args) = do
        newCallee <- callee
        newArgs <- sequence args
        pure $ Ast.Call newCallee newArgs
    resolve (Ast.LiteralF lit) = pure $ Ast.Literal lit
    resolve (Ast.LogicalF op left right) = do
        newL <- left
        Ast.Logical op newL <$> right
    resolve (Ast.UnaryF op operand) = Ast.Unary op <$> operand
    resolve (Ast.FunExprF f) = Ast.FunExpr <$> resolveFunc (pure <$> f) Ast.FunKind
    resolve (Ast.GetF {..}) = flip Ast.Get getNameF <$> getObjectF
    resolve (Ast.SetF {..}) = do
        obj <- setObjectF
        Ast.Set obj setNameF <$> setValueF
    resolve (Ast.ThisF _) = Ast.This <$> resolveLocal "this"
    resolve (Ast.SuperF method level) = Ast.Super method <$> resolveLocal "super"

resolveLocal :: String -> Solver (Maybe Int)
resolveLocal name = do
    scopes <- ask
    fmap fst <$> liftIO (findM (hasName name . snd) (zip [0 ..] scopes))
  where
    hasName :: String -> HashTable String Initialized -> IO Bool
    hasName name table = isJust <$> H.lookup table name

declare :: String -> Solver ()
declare name = do
    scopes <- ask
    -- liftIO $ print ("Declaring `" ++ name ++ "` with scopes " ++ show scopes)
    case scopes of
        (sc : _) -> do
            val <- liftIO $ H.lookup sc name
            when (isJust val) $ throwE (DuplicateVarName name)
            liftIO $ H.insert sc name False
        _ -> pure ()

define :: String -> Solver ()
define name = do
    scopes <- ask
    -- liftIO $ print ("Defining `" ++ name ++ "` with scopes " ++ show scopes)
    case scopes of
        (sc : _) -> liftIO $ H.insert sc name True
        _ -> pure ()

--- >>> take 5 [0..]
-- [0,1,2,3,4]
