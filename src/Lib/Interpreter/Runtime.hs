module Lib.Interpreter.Runtime (
    Runtime,
    Completion (..),
    ask,
    asks,
    local,
    tell,
    print,
    throwE,
    except,
    define,
    getVar,
    assign,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Ex
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State (modify)
import Control.Monad.Trans.State.Lazy (StateT, mapStateT)
import qualified Lib.Ast as Ast
import Lib.Interpreter.Error (RuntimeError)
import qualified Lib.Interpreter.ExecCtx as EC
import Lib.Interpreter.ExecCtx.Environment (Environment)
import qualified Lib.Interpreter.LoxValue as LV
import Prelude hiding (print)
import qualified Prelude as P

type Runtime = StateT Completion (ReaderT EC.ExecCtx (ExceptT RuntimeError IO))

data Completion = Normal | Return LV.LoxValue | Continue | Break deriving (Eq)

instance Show Completion where
    show Normal = "normal"
    show (Return _) = "return"
    show Continue = "continue"
    show Break = "break"

instance Semigroup Completion where
    Normal <> Normal = Normal
    c <> Normal = c
    Normal <> c = c
    c <> _ = c

instance Monoid Completion where
    mempty = Normal

ask :: Runtime EC.ExecCtx
ask = lift R.ask

asks :: (EC.ExecCtx -> a) -> Runtime a
asks f = lift $ R.asks f

local :: (EC.ExecCtx -> EC.ExecCtx) -> Runtime a -> Runtime a
local = mapStateT . R.local

tell :: Completion -> Runtime ()
tell c = modify (<> c)

print :: Show a => a -> Runtime ()
print = liftIO . P.print

throwE :: RuntimeError -> Runtime a
throwE = lift . lift . Ex.throwE

except :: Either RuntimeError a -> Runtime a
except eit = lift (lift $ Ex.except eit)

define :: EC.ExecCtx -> String -> LV.LoxValue -> Runtime ()
define ctx name val = liftIO $ EC.define ctx name val

getVar :: EC.ExecCtx -> Ast.Variable -> Runtime LV.LoxValue
getVar ctx var = lift . lift $ EC.getVar ctx var

assign :: EC.ExecCtx -> Ast.Variable -> LV.LoxValue -> Runtime ()
assign ctx var val = (lift . lift) $ EC.assign ctx var val


