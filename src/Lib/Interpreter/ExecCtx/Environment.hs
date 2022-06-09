module Lib.Interpreter.ExecCtx.Environment (Environment, new, define, getVar, assign, ancestor, printDepth) where

import Control.Error.Util (note)
import Control.Monad (join)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import qualified Data.HashTable.IO as H
import Data.Hashable (Hashable)
import Data.Maybe (isJust, isNothing)
import Lib.Interpreter.Error (RuntimeError (InvalidAncestorIdx, ReferenceError))

-- import qualified Lib.Interpreter.LoxValue as LV

type HashTable k v = H.BasicHashTable k v

data Environment k v = Environment
    { enclosing :: Maybe (Environment k v),
      contextValues :: HashTable k v
    }

new :: Maybe (Environment k v) -> IO (Environment k v)
new env = Environment env <$> H.new

define :: (Eq k, Hashable k) => Environment k v -> k -> v -> IO ()
define = H.insert . contextValues

tryGetSelf :: (Eq k, Hashable k) => Environment k v -> k -> IO (Maybe v)
tryGetSelf = H.lookup . contextValues

getVar :: (Eq k, Hashable k, Show k) => Environment k v -> k -> ExceptT RuntimeError IO v
getVar env str = do
    val <- lift $ tryGetSelf env str
    case val of
        Just val -> pure val
        Nothing -> throwE (ReferenceError (show str))

assign :: (Eq k, Hashable k, Show k) => Environment k v -> k -> v -> ExceptT RuntimeError IO ()
assign env str val = do
    isDefined <- lift $ isJust <$> tryGetSelf env str
    if isDefined
        then lift $ define env str val
        else throwE (ReferenceError (show str))

ancestor :: Monad m => Environment k v -> Int -> ExceptT RuntimeError m (Environment k v)
ancestor inner totalHops = ancestor' inner totalHops
  where
    ancestor' :: Monad m => Environment k v -> Int -> ExceptT RuntimeError m (Environment k v)
    ancestor' env h | h < 0 = throwE (InvalidAncestorIdx totalHops)
    ancestor' env 0 = pure env
    ancestor' env h = case enclosing env of
        Just outer -> ancestor' outer (h - 1)
        Nothing -> throwE (InvalidAncestorIdx totalHops)

printDepth :: (Eq k, Hashable k, Show k) => Environment k v -> IO ()
printDepth env = printDepth' env 0
  where
    printDepth' env depth = case enclosing env of
        Just outer -> printDepth' outer (depth + 1)
        Nothing -> print ("Depth: " ++ show depth)
