{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Interpreter (interpret, evaluate) where

import Control.Error (fromMaybe, isJust, isNothing)
import Control.Monad (unless, void, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Control.Monad.Trans.Except as Ex
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, execStateT, get, mapStateT, modify, put, runStateT, withStateT)
import Data.Dynamic (toDyn)
import Data.Functor.Foldable (Recursive (cata), cataA)
import Data.Functor.Identity (Identity)
import qualified Data.HashTable.IO as H
import Data.List.Extra (lower)
import Flow ((.>), (<.), (<|), (|>))
import qualified Lib.Ast as Ast
import Lib.Interpreter.Error (RuntimeError (..))
import qualified Lib.Interpreter.ExecCtx as EC
import qualified Lib.Interpreter.ExecCtx.Environment as Env
import Lib.Interpreter.LoxCallable (LoxCallable (..))
import qualified Lib.Interpreter.LoxValue as LV
import Lib.Interpreter.Runtime (Runtime)
import qualified Lib.Interpreter.Runtime as RT
import Prelude hiding (print)
import qualified Prelude as P

interpret ::
    -- | context where the statements will be run
    EC.ExecCtx ->
    -- | Compiled statements
    [Ast.Stmt] ->
    ExceptT RuntimeError IO ()
interpret exec stmts = runAllWithEnv (execMany stmts) exec
  where
    runAllWithEnv :: Runtime () -> EC.ExecCtx -> ExceptT RuntimeError IO ()
    runAllWithEnv rt exec = void (runReaderT (evalStateT rt RT.Normal) exec)

evaluate :: EC.ExecCtx -> Ast.Expr -> ExceptT RuntimeError IO LV.LoxValue
evaluate exec expr = runAllWithEnv (evalExpr expr) exec
  where
    runAllWithEnv :: Runtime LV.LoxValue -> EC.ExecCtx -> ExceptT RuntimeError IO LV.LoxValue
    runAllWithEnv rt = runReaderT (evalStateT rt RT.Normal)

execMany :: [Ast.Stmt] -> Runtime ()
execMany stmts = sequence_ (execStmtChecked <$> stmts)
  where
    execStmtChecked :: Ast.Stmt -> Runtime ()
    execStmtChecked stmt = do
        comp <- get
        case comp of
            RT.Normal -> execStmt stmt
            _ -> pure ()

execStmt :: Ast.Stmt -> Runtime ()
execStmt (Ast.ExprStmt expr) = (expr |> evalExpr) >>= (pure () .> pure)
execStmt (Ast.PrintStmt expr) = (expr |> evalExpr) >>= RT.print
execStmt (Ast.VarStmt {Ast.varName, Ast.varInit}) = do
    init <- maybe (pure LV.Nil) evalExpr varInit
    exec <- RT.ask
    RT.define exec varName init
execStmt (Ast.BlockStmt stmts) = do
    exec <- RT.ask
    localExec <- liftIO $ EC.localCtx exec
    RT.local (const localExec) (execMany stmts)
execStmt (Ast.IfStmt {Ast.ifCond, Ast.ifTrueBranch, Ast.ifFalseBranch}) = do
    ifCond <- evalExpr ifCond
    if LV.isTruthy ifCond
        then execStmt (Ast.BlockStmt ifTrueBranch)
        else maybe (pure ()) (execStmt . Ast.BlockStmt) ifFalseBranch
execStmt (Ast.LoopStmt stmts) = loop'
  where
    loop' :: Runtime ()
    loop' = do
        execStmt (Ast.BlockStmt stmts)
        comp <- get
        case comp of
            RT.Continue -> put RT.Normal >> loop'
            RT.Break -> put RT.Normal
            RT.Normal -> loop'
            RT.Return _ -> pure ()
execStmt Ast.BreakStmt = RT.tell RT.Break
execStmt Ast.ContinueStmt = RT.tell RT.Continue
execStmt (Ast.FunStmt func@Ast.Function {Ast.funcName}) = do
    ctx <- RT.ask
    loxFunc <- toLoxFunc func No
    let fun = LV.Fun loxFunc
    RT.define ctx (fromMaybe "" funcName) fun
execStmt (Ast.ReturnStmt val) = do
    evalExpr val >>= RT.tell . RT.Return
execStmt (Ast.ClassStmt {..}) = do
    super <- mapM resolveSuper classSuper
    ctx <- RT.ask
    RT.define ctx className LV.Nil

    superExec <- case super of
        Just superClass -> do
            localExec <- liftIO $ EC.localCtx ctx
            RT.define localExec "super" (LV.Class superClass)
            pure localExec
        Nothing -> pure ctx

    loxClass <- RT.local (const superExec) $ do
        let names = fromMaybe "" . Ast.funcName <$> classMethods
        loxFuncs <- mapM (`toLoxFunc` No) classMethods
        methods <- (liftIO . H.fromList) $ zipWith setInitializer names loxFuncs
        pure LV.LoxClass {LV.loxClassName = className, LV.loxClassMethods = methods, LV.loxClassSuper = super}

    RT.assign ctx (Ast.Variable className (Just 0)) (LV.Class loxClass)
  where
    setInitializer :: String -> LV.LoxFunc -> (String, LV.LoxFunc)
    setInitializer n@"init" fun@LV.VirtualFunc {} = (n, fun {LV.virtualInit = True})
    setInitializer n f = (n, f)

    resolveSuper :: Ast.Variable -> Runtime LV.LoxClass
    resolveSuper var = do
        super <- evalExpr $ Ast.Var var
        case super of
            LV.Class cl -> pure cl
            _ -> RT.throwE InvalidSuperClass

data AllowAnon = Yes | No deriving (Eq)

toLoxFunc :: Ast.Function Ast.Stmt -> AllowAnon -> Runtime LV.LoxFunc
toLoxFunc (Ast.Function {..}) allowAnon = do
    when (isNothing funcName && allowAnon == No) (RT.throwE InvalidAnonFun)
    ctx <- RT.ask
    let env = EC.current ctx
    pure $ LV.VirtualFunc funcName funcParams funcBody env False

evalExpr :: Ast.Expr -> Runtime LV.LoxValue
evalExpr = cataA (sequence >=> eval)
  where
    eval :: Ast.ExprF LV.LoxValue -> Runtime LV.LoxValue
    eval (Ast.LiteralF lit) = pure (LV.fromLiteral lit)
    eval (Ast.UnaryF Ast.Negative expr) = RT.except $ LV.negative expr
    eval (Ast.UnaryF Ast.Not expr) = pure (LV.not expr)
    eval (Ast.BinaryF Ast.Minus x y) = RT.except $ LV.sub x y
    eval (Ast.BinaryF Ast.Plus x y) = RT.except $ LV.add x y
    eval (Ast.BinaryF Ast.Div x y) = RT.except $ LV.div x y
    eval (Ast.BinaryF Ast.Times x y) = RT.except $ LV.mult x y
    eval (Ast.BinaryF Ast.Greater x y) = RT.except $ LV.greater x y
    eval (Ast.BinaryF Ast.GreaterEqual x y) = RT.except $ LV.greaterEq x y
    eval (Ast.BinaryF Ast.Less x y) = RT.except $ LV.less x y
    eval (Ast.BinaryF Ast.LessEqual x y) = RT.except $ LV.lessEq x y
    eval (Ast.BinaryF Ast.Equal x y) = pure $ LV.Boolean $ x == y
    eval (Ast.BinaryF Ast.NotEqual x y) = pure $ LV.Boolean $ x /= y
    eval (Ast.VarF var) = RT.ask >>= flip RT.getVar var
    eval (Ast.AssignF {Ast.lValueF, Ast.rValueF}) = do
        exec <- RT.ask
        RT.assign exec lValueF rValueF
        pure rValueF
    eval (Ast.LogicalF op left right) = pure $ case op of
        Ast.Or | LV.isTruthy left -> left
        Ast.And | not . LV.isTruthy $ left -> left
        _ -> right
    eval (Ast.CallF (LV.Fun f) args) = checkedCall f args
    eval (Ast.CallF (LV.Class cl) args) = checkedCall cl args
    eval (Ast.CallF _ _) = RT.throwE InvalidCallee
    eval (Ast.FunExprF func) = LV.Fun <$> toLoxFunc func Yes
    eval (Ast.GetF {Ast.getObjectF = LV.Object obj, ..}) = (lift . lift) $ LV.getProp obj getNameF
    eval (Ast.GetF {}) = RT.throwE InvalidObjectAccess
    eval (Ast.SetF {Ast.setObjectF = LV.Object obj, ..}) = liftIO $ LV.setProp obj setNameF setValueF >> pure setValueF
    eval (Ast.SetF {}) = RT.throwE InvalidObjectAccess
    eval (Ast.ThisF lvl) = RT.ask >>= flip RT.getVar (Ast.Variable "this" lvl)
    eval (Ast.SuperF method level) = do
        exec <- RT.ask
        superVar <- RT.getVar exec (Ast.Variable "super" level)
        super <- case superVar of
            LV.Class cl -> pure cl
            _ -> RT.throwE InvalidSuperReference

        objectVar <- RT.getVar exec (Ast.Variable "this" (subtract 1 <$> level))
        object <- case objectVar of
            LV.Object obj -> pure obj
            _ -> RT.throwE InvalidObjectReference

        methodVar <- liftIO $ LV.findMethod super method

        method <- maybe (RT.throwE (UndefinedProperty method)) pure methodVar

        liftIO (LV.Fun <$> LV.bind method object)

instance LoxCallable LV.LoxClass where
    invoke cl args = do
        fields <- liftIO H.new
        let obj = LV.LoxObject {LV.loxObjectClass = cl, LV.loxObjectFields = fields}

        init <- liftIO $ LV.findMethod cl "init"
        case init of
            Just fun -> liftIO (LV.bind fun obj) >>= flip checkedCall args
            Nothing -> pure $ LV.Object obj
    arity cl = do
        init <- liftIO $ LV.findMethod cl "init"
        case init of
            Just fun -> arity fun
            Nothing -> pure 0

instance LoxCallable LV.LoxFunc where
    invoke (LV.NativeFunc f _) args = liftIO $ f args
    invoke fun@(LV.VirtualFunc {..}) args = do
        ctx <- RT.asks $ flip EC.setCurrent virtualClosure
        funCtx <- liftIO $ EC.localCtx ctx
        RT.local (const funCtx) $ do
            localCtx <- RT.ask
            let assoc = zip virtualArgs args
            mapM_ (uncurry $ RT.define localCtx) assoc
            case virtualName of
                Just name -> RT.define localCtx name (LV.Fun fun)
                Nothing -> pure ()
            execMany virtualStmts
        comp <- get
        case (comp, virtualInit) of
            (_, True) -> put RT.Normal >> (lift . lift) (Env.getVar virtualClosure "this")
            (RT.Return val, False) -> put RT.Normal >> pure val
            (RT.Normal, False) -> pure LV.Nil
            (a, _) -> RT.throwE (ControlFlow $ show a)

    arity fun = pure $ LV.loxFuncArity fun