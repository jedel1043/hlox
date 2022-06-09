module Main where

import Control.Error (ExceptT)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, withExceptT)

import Data.Either.Extra (mapLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO ()
import Data.Text.IO.Utf8 as Utf8 (readFile)

import Flow ((.>), (<.), (<|), (|>))
import Main.Utf8 (withUtf8)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdout)

import qualified Lib.Ast as Ast
import qualified Lib.Err as Err
import Lib.Interpreter (evaluate, interpret)
import qualified Lib.Interpreter.ExecCtx as EC
import Lib.Lexer (tokenize)
import Lib.Parser (parse)
import Lib.Resolver (resolve)

main :: IO ()
main = withUtf8 $ do
    args <- getArgs
    let action = case args of
            [] -> promptMode
            [x] -> fileMode x
            _ -> do
                hPutStrLn stderr "Usage: hlox [script]"
                exitWith $ ExitFailure 64
    action

promptMode :: IO ()
promptMode = do
    ctx <- EC.new
    promptMode' ctx
  where
    promptMode' :: EC.ExecCtx -> IO ()
    promptMode' ctx = do
        line <- putStr "> " *> hFlush stdout *> getLine
        unless (line == "") $ do
            result <- run ctx line True
            case result of
                Left error -> handleError error False
                _ -> pure ()
            promptMode' ctx

fileMode :: String -> IO ()
fileMode path = do
    ctx <- EC.new
    text <- Utf8.readFile path
    let script = Text.unpack text
    result <- run ctx script False
    case result of
        Left error -> handleError error True
        _ -> pure ()

run :: EC.ExecCtx -> String -> Bool -> IO (Either Err.InterpreterError ())
run ctx str isRepl = runExceptT $ do
    tokens <- except <. mapLeft Err.LexError <. tokenize <| str
    stmts <- except <. mapLeft Err.ParseError <. parse <| tokens
    resolvedStmts <- withExceptT Err.ReferenceError <. resolve <| stmts
    -- lift $ print resolvedStmts
    eval ctx resolvedStmts isRepl
  where
    eval :: EC.ExecCtx -> [Ast.Stmt] -> Bool -> ExceptT Err.InterpreterError IO ()
    eval env stmts True
        | [Ast.ExprStmt expr] <- stmts =
            (expr |> evaluate env .> withExceptT Err.RuntimeError) >>= (print .> lift)
    eval env stmts _ = stmts |> interpret env .> withExceptT Err.RuntimeError

handleError :: Err.InterpreterError -> Bool -> IO ()
handleError error exit = do
    hPrint stderr error
    when exit $ case error of
        Err.LexError _ -> exitWith $ ExitFailure 64
        Err.ParseError _ -> exitWith $ ExitFailure 64
        Err.ReferenceError _ -> exitWith $ ExitFailure 64
        Err.RuntimeError _ -> exitWith $ ExitFailure 65