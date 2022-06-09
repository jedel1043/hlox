{-# LANGUAGE NamedFieldPuns #-}

module Lib.Interpreter.LoxCallable (LoxCallable (..)) where

import Control.Monad (when)
import qualified Lib.Interpreter.Error as Err
import qualified Lib.Interpreter.ExecCtx as EC
import qualified Lib.Interpreter.LoxValue as LV
import Lib.Interpreter.Runtime

class LoxCallable a where
    checkedCall :: a -> [LV.LoxValue] -> Runtime LV.LoxValue
    checkedCall callee args = do
        let callerLen = length args
        calleeLen <- arity callee
        when
            (callerLen /= calleeLen)
            (throwE (Err.InvalidArgsLen {Err.callerLen, Err.calleeLen}))
        invoke callee args
    invoke :: a -> [LV.LoxValue] -> Runtime LV.LoxValue
    arity :: a -> Runtime Int