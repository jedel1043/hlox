{-# LANGUAGE NamedFieldPuns #-}

module Lib.Interpreter.ExecCtx (ExecCtx, globals, current, setCurrent, new, define, getVar, assign, localCtx) where

import Control.Error.Util (note)
import Control.Monad (join)
import Control.Monad.Extra (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Maybe (isJust)
import Data.STRef (newSTRef)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Lib.Ast as Ast
import Lib.Interpreter.Error (RuntimeError (ReferenceError))
import Lib.Interpreter.ExecCtx.Environment (ancestor, printDepth)
import qualified Lib.Interpreter.ExecCtx.Environment as Env
import Lib.Interpreter.LoxValue (LoxValue)
import qualified Lib.Interpreter.LoxValue as LV

type LoxEnv = Env.Environment String LoxValue

data ExecCtx = ExecCtx {globals_ :: LoxEnv, current_ :: LoxEnv}

globals :: ExecCtx -> LoxEnv
globals = globals_

current :: ExecCtx -> LoxEnv
current = current_

setCurrent :: ExecCtx -> LoxEnv -> ExecCtx
setCurrent ctx env = ctx {current_ = env}

new :: IO ExecCtx
new = do
    global <- Env.new Nothing
    Env.define global "clock" $ LV.Fun (LV.NativeFunc currentTime 0)
    pure $ ExecCtx global global
  where
    currentTime :: [LV.LoxValue] -> IO LV.LoxValue
    currentTime _ =
        LV.Int
            . floor
            . (1e12 *)
            . nominalDiffTimeToSeconds
            . utcTimeToPOSIXSeconds
            <$> getCurrentTime

define :: ExecCtx -> String -> LV.LoxValue -> IO ()
define = Env.define . current_

getVar :: ExecCtx -> Ast.Variable -> ExceptT RuntimeError IO LV.LoxValue
getVar ctx var@(Ast.Variable {Ast.variableName, Ast.variableLevel}) = do
    -- lift . print $ "GetVar: " ++ show var
    -- lift . printDepth $ current_ ctx
    env <- case variableLevel of
        Just hops -> ancestor (current_ ctx) hops
        Nothing -> pure (globals_ ctx)
    -- lift . printDepth $ env
    Env.getVar env variableName

assign :: ExecCtx -> Ast.Variable -> LoxValue -> ExceptT RuntimeError IO ()
assign ctx var@(Ast.Variable {Ast.variableName, Ast.variableLevel}) val = do
    -- lift . print $ "Assign: " ++ show var
    -- lift . printDepth $ current_ ctx
    env <- case variableLevel of
        Just hops -> ancestor (current_ ctx) hops
        Nothing -> pure (globals_ ctx)
    -- lift . printDepth $ env
    Env.assign env variableName val

localCtx :: ExecCtx -> IO ExecCtx
localCtx ctx = do
    local <- Env.new $ Just (current_ ctx)
    pure $ ctx {current_ = local}