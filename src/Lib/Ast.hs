{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Ast (
    Stmt (..),
    StmtF (..),
    Expr (..),
    ExprF (..),
    Literal (..),
    UnaryOp (..),
    BinaryOp (..),
    LogicalOp (..),
    PrettyExpr (PrettyExpr),
    Function (..),
    FunType (..),
    Variable (..),
) where

import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.Extra (intercalate, lower)

data Literal
    = Double Double
    | Int Int
    | String String
    | Boolean Bool
    | Nil
    deriving (Eq)

instance Show Literal where
    show (Double double) = show double
    show (Int int) = show int
    show (String string) = string
    show (Boolean bool) = lower . show $ bool
    show Nil = "nil"

data UnaryOp = Negative | Not deriving (Eq)

instance Show UnaryOp where
    show Negative = "-"
    show Not = "!"

data BinaryOp
    = Equal
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Plus
    | Minus
    | Times
    | Div
    deriving (Eq)

instance Show BinaryOp where
    show Equal = "=="
    show NotEqual = "!="
    show Less = "<"
    show LessEqual = "<="
    show Greater = ">"
    show GreaterEqual = ">="
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Div = "/"

data LogicalOp
    = And
    | Or

instance Show LogicalOp where
    show And = "and"
    show Or = "or"

data Variable = Variable {variableName :: String, variableLevel :: Maybe Int} deriving (Show)

data Expr
    = Literal Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | Var Variable
    | Assign {lValue :: Variable, rValue :: Expr}
    | Logical LogicalOp Expr Expr
    | FunExpr (Function Stmt)
    | Call {callee :: Expr, callArgs :: [Expr]}
    | Get {getObject :: Expr, getName :: String}
    | Set {setObject :: Expr, setName :: String, setValue :: Expr}
    | Super {superMethod :: String, superLevel :: Maybe Int}
    | This (Maybe Int)
    deriving (Show)

data FunType = FunKind | MethodKind | InitKind
instance Show FunType where
    show FunKind = "function"
    show MethodKind = "method"
    show InitKind = "initializer"

data Function a = Function {funcName :: Maybe String, funcParams :: [String], funcBody :: [a]}
    deriving (Show, Functor, Foldable, Traversable)

newtype PrettyExpr = PrettyExpr Expr

data Stmt
    = ExprStmt Expr
    | PrintStmt Expr
    | VarStmt {varName :: String, varInit :: Maybe Expr}
    | IfStmt {ifCond :: Expr, ifTrueBranch :: [Stmt], ifFalseBranch :: Maybe [Stmt]}
    | LoopStmt [Stmt]
    | BreakStmt
    | ContinueStmt
    | ReturnStmt Expr
    | FunStmt (Function Stmt)
    | BlockStmt [Stmt]
    | ClassStmt {className :: String, classSuper :: Maybe Variable, classMethods :: [Function Stmt]}
    deriving (Show)

makeBaseFunctor ''Expr

makeBaseFunctor ''Stmt

instance Show PrettyExpr where
    show (PrettyExpr expr) = cata prettyPrint expr
      where
        prettyPrint :: ExprF String -> String
        prettyPrint (LiteralF l) = show l
        prettyPrint (UnaryF op expr) = "( " ++ show op ++ " " ++ expr ++ " )"
        prettyPrint (BinaryF op e1 e2) = "( " ++ show op ++ " " ++ e1 ++ " " ++ e2 ++ " )"
        prettyPrint (VarF name) = show name
        prettyPrint (AssignF {..}) = show lValueF ++ " = " ++ rValueF
        prettyPrint (LogicalF op e1 e2) = "( " ++ show op ++ " " ++ e1 ++ " " ++ e2 ++ " )"
        prettyPrint (CallF {..}) = calleeF ++ "( " ++ intercalate ", " callArgsF ++ " )"
        prettyPrint (FunExprF Function {funcName}) = maybe "anon fun" ("fun " ++) funcName
        prettyPrint (GetF {..}) = getObjectF ++ "." ++ getNameF
        prettyPrint (SetF {..}) = setObjectF ++ "." ++ setNameF ++ " = " ++ setValueF
        prettyPrint (SuperF {superMethodF}) = "super." ++ superMethodF
        prettyPrint (ThisF _) = "this"