module Lib.Err (InterpreterError (..)) where

import Data.List (intercalate)
import Lib.Interpreter.Error (RuntimeError)
import Lib.Lexer (LexError)
import Lib.Parser (ParseError)
import Lib.Resolver (ReferenceError)

data InterpreterError = LexError [LexError] | ParseError ParseError | RuntimeError RuntimeError | ReferenceError ReferenceError

instance Show InterpreterError where
    show (LexError err) = "Tokenizer error!\n" ++ (intercalate "\n" . map show $ err)
    show (ParseError err) = "Parse error: " ++ show err
    show (ReferenceError err) = "Reference error: " ++ show err
    show (RuntimeError err) = "Runtime error: " ++ show err