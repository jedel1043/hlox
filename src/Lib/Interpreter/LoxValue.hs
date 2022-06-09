{-# LANGUAGE NamedFieldPuns #-}

module Lib.Interpreter.LoxValue (
    LoxValue (..),
    LoxFunc (..),
    LoxClass (..),
    LoxObject (..),
    fromLiteral,
    asBoolean,
    asDouble,
    asString,
    asPrimitive,
    toString,
    isTruthy,
    negative,
    not,
    add,
    sub,
    mult,
    div,
    less,
    greater,
    lessEq,
    greaterEq,
    loxFuncArity,
    getProp,
    setProp,
    findMethod,
    bind,
) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.HashTable.IO as H
import Data.List.Extra (lower)
import Data.Maybe (isJust)
import Flow ((.>), (<.), (<|))
import GHC.Float (int2Double)
import qualified Lib.Ast as Ast
import Lib.Interpreter.Error (
    RuntimeError (
        ComplexObjectStringCast,
        InvalidNumberOperand,
        InvalidPrimitiveOperand,
        InvalidSumOperand,
        UndefinedProperty
    ),
 )
import qualified Lib.Interpreter.ExecCtx.Environment as Env
import Prelude hiding (div, not, sum)
import qualified Prelude as P

data LoxFunc
    = NativeFunc {nativeFunc :: [LoxValue] -> IO LoxValue, nativeFuncArity :: Int}
    | VirtualFunc
        { virtualName :: Maybe String,
          virtualArgs :: [String],
          virtualStmts :: [Ast.Stmt],
          virtualClosure :: Env.Environment String LoxValue,
          virtualInit :: Bool
        }

loxFuncArity :: LoxFunc -> Int
loxFuncArity NativeFunc {nativeFuncArity} = nativeFuncArity
loxFuncArity VirtualFunc {virtualArgs} = length virtualArgs

instance Show LoxFunc where
    show (NativeFunc _ _) = "<native fun>"
    show (VirtualFunc {virtualName}) = maybe "<anon fun>" (("<fun " ++) .> (++ ">")) virtualName

data LoxValue
    = Class LoxClass
    | Object LoxObject
    | Fun LoxFunc
    | Boolean Bool
    | Double Double
    | Int Int
    | String String
    | Nil

instance Eq LoxValue where
    Nil == Nil = True
    x == y | (Just x, Just y) <- (asBoolean x, asBoolean y) = x == y
    x == y | (Just x, Just y) <- (asDouble x, asDouble y) = x == y
    x == y | (Just x, Just y) <- (asString x, asString y) = x == y
    _ == _ = False

instance Show LoxValue where
    show (Class cl) = show cl
    show (Object obj) = show obj
    show (Boolean bool) = lower . show $ bool
    show (Double dbl) = show dbl
    show (Int int) = show int
    show (String str) = str
    show (Fun func) = "fun " ++ show func ++ ": " ++ show (loxFuncArity func)
    show Nil = "nil"

fromLiteral :: Ast.Literal -> LoxValue
fromLiteral (Ast.Boolean bool) = Boolean bool
fromLiteral (Ast.Double r) = Double r
fromLiteral (Ast.Int i) = Int i
fromLiteral (Ast.String str) = String str
fromLiteral Ast.Nil = Nil

asBoolean :: LoxValue -> Maybe Bool
asBoolean (Boolean bool) = Just bool
-- asBoolean (Any any) = fromDynamic any
asBoolean _ = Nothing

asDouble :: LoxValue -> Maybe Double
asDouble (Double dbl) = Just dbl
asDouble (Int int) = Just (int2Double int)
-- asDouble (Any any) = fromDynamic any
asDouble _ = Nothing

asInt :: LoxValue -> Maybe Int
asInt (Int int) = Just int
-- asInt (Any any) = fromDynamic any
asInt _ = Nothing

asString :: LoxValue -> Maybe String
asString (String str) = Just str
-- asString (Any any) = fromDynamic any
asString _ = Nothing

asPrimitive :: LoxValue -> LoxValue
asPrimitive val | Just str <- asString val = String str
asPrimitive val | Just bool <- asBoolean val = Boolean bool
asPrimitive val | Just num <- asInt val = Int num
asPrimitive val | Just num <- asDouble val = Double num
asPrimitive Nil = Nil
asPrimitive obj = obj

toString :: LoxValue -> Either RuntimeError String
toString val | Just str <- asString val = Right str
toString val | Just bool <- asBoolean val = Right <. lower <. show <| bool
toString val | Just num <- asInt val = Right <. show <| num
toString val | Just num <- asDouble val = Right <. show <| num
toString Nil = Right "nil"
toString _ = Left ComplexObjectStringCast

isTruthy :: LoxValue -> Bool
isTruthy val | Just bool <- asBoolean val = bool
isTruthy Nil = False
isTruthy _ = True

not :: LoxValue -> LoxValue
not = Boolean <. P.not <. isTruthy

negative :: LoxValue -> Either RuntimeError LoxValue
negative val | Just num <- asInt val = Right (Int (-num))
negative val | Just num <- asDouble val = Right (Double (-num))
negative _ = Left InvalidNumberOperand

add :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
add x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Int (x + y))
add x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Double (x + y))
add x y | Just x <- asString x = String . (x ++) <$> toString y
add x y | Just y <- asString y = String . (++ y) <$> toString x
add _ _ = Left InvalidSumOperand

sub :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
sub x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Int (x - y))
sub x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Double (x - y))
sub _ _ = Left InvalidNumberOperand

mult :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
mult x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Int (x * y))
mult x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Double (x * y))
mult _ _ = Left InvalidNumberOperand

div :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
div x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Int (x `P.div` y))
div x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Double (x / y))
div _ _ = Left InvalidNumberOperand

less :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
less x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Boolean (x < y))
less x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Boolean (x < y))
less x y | (Just x, Just y) <- (asString x, asString y) = Right (Boolean (x < y))
less x y | (Just x, Just y) <- (asBoolean x, asBoolean y) = Right (Boolean (x < y))
less Nil Nil = Right (Boolean (() < ()))
less _ _ = Left InvalidPrimitiveOperand

greater :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
greater x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Boolean (x > y))
greater x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Boolean (x > y))
greater x y | (Just x, Just y) <- (asString x, asString y) = Right (Boolean (x > y))
greater x y | (Just x, Just y) <- (asBoolean x, asBoolean y) = Right (Boolean (x > y))
greater Nil Nil = Right (Boolean (() > ()))
greater _ _ = Left InvalidPrimitiveOperand

lessEq :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
lessEq x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Boolean (x <= y))
lessEq x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Boolean (x <= y))
lessEq x y | (Just x, Just y) <- (asString x, asString y) = Right (Boolean (x <= y))
lessEq x y | (Just x, Just y) <- (asBoolean x, asBoolean y) = Right (Boolean (x <= y))
lessEq Nil Nil = Right (Boolean (() <= ()))
lessEq _ _ = Left InvalidPrimitiveOperand

greaterEq :: LoxValue -> LoxValue -> Either RuntimeError LoxValue
greaterEq x y | (Just x, Just y) <- (asInt x, asInt y) = Right (Boolean (x >= y))
greaterEq x y | (Just x, Just y) <- (asDouble x, asDouble y) = Right (Boolean (x >= y))
greaterEq x y | (Just x, Just y) <- (asString x, asString y) = Right (Boolean (x >= y))
greaterEq x y | (Just x, Just y) <- (asBoolean x, asBoolean y) = Right (Boolean (x >= y))
greaterEq Nil Nil = Right (Boolean (() >= ()))
greaterEq _ _ = Left InvalidPrimitiveOperand

type HashTable k v = H.BasicHashTable k v

data LoxClass = LoxClass
    { loxClassName :: String,
      loxClassMethods :: HashTable String LoxFunc,
      loxClassSuper :: Maybe LoxClass
    }

instance Show LoxClass where
    show (LoxClass {loxClassName}) = loxClassName

data LoxObject = LoxObject {loxObjectClass :: LoxClass, loxObjectFields :: HashTable String LoxValue}

instance Show LoxObject where
    show (LoxObject {loxObjectClass}) = show loxObjectClass ++ " instance"

getProp :: LoxObject -> String -> ExceptT RuntimeError IO LoxValue
getProp obj@LoxObject {loxObjectClass = clas, loxObjectFields} propName = do
    prop <- liftIO $ H.lookup loxObjectFields propName
    case prop of
        Just value -> pure value
        Nothing -> do
            method <- lift $ findMethod clas propName
            case method of
                Just func -> Fun <$> lift (bind func obj)
                Nothing -> throwE $ UndefinedProperty propName

findMethod :: LoxClass -> String -> IO (Maybe LoxFunc)
findMethod LoxClass {loxClassMethods = methods, loxClassSuper} method = do
    val <- H.lookup methods method
    if isJust val
        then pure val
        else join <$> mapM ((`H.lookup` method) . loxClassMethods) loxClassSuper

bind :: LoxFunc -> LoxObject -> IO LoxFunc
bind fun@VirtualFunc {virtualClosure} obj = do
    thisEnv <- Env.new (Just virtualClosure)
    Env.define thisEnv "this" (Object obj)
    pure fun {virtualClosure = thisEnv}
bind fun _ = pure fun

setProp :: LoxObject -> String -> LoxValue -> IO ()
setProp (LoxObject {loxObjectFields}) = H.insert loxObjectFields
