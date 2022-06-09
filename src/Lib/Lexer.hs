{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Lib.Lexer (
    Token (..),
    TokenType (..),
    KW (..),
    LexError,
    tokenize,
    isKeyword,
    Position (..),
) where

import Control.Error (AllE (runAllE))
import Control.Error.Util (AllE (AllE))
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy (RWS, ask, evalRWS, execRWS, get, gets, modify, put, tell)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List.Extra (length, singleton, (!?), (++))
import Data.Maybe (isNothing)
import Flow ((.>), (<.), (<|), (|>))
import Text.Read (readMaybe)
import Prelude hiding (span)

data TokenType
    = -- Single character tokens
      LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Colon
    | Semicolon
    | Slash
    | Star
    | -- Single/Double character tokens
      Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | -- Literals
      Identifier String
    | Keyword KW
    | String String
    | Double Double
    | Int Int
    | -- EOF
      Eof
    deriving (Show, Eq)

isKeyword :: TokenType -> Bool
isKeyword (Keyword _) = Prelude.True
isKeyword _ = Prelude.False

data KW
    = And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Break
    | Continue
    | Loop
    deriving (Show, Eq)

keywordKind :: String -> Maybe KW
keywordKind "and" = Just And
keywordKind "class" = Just Class
keywordKind "else" = Just Else
keywordKind "false" = Just Lib.Lexer.False
keywordKind "fun" = Just Fun
keywordKind "for" = Just For
keywordKind "if" = Just If
keywordKind "nil" = Just Nil
keywordKind "or" = Just Or
keywordKind "print" = Just Print
keywordKind "return" = Just Return
keywordKind "super" = Just Super
keywordKind "this" = Just This
keywordKind "true" = Just Lib.Lexer.True
keywordKind "var" = Just Var
keywordKind "while" = Just While
keywordKind "break" = Just Break
keywordKind "continue" = Just Continue
keywordKind "loop" = Just Loop
keywordKind _ = Nothing

data Token = Token
    { tokenType :: TokenType,
      tokenSpan :: Position
    }
    deriving (Show)

type Source = String

data Position = Position
    { positionStart :: Int,
      positionCurrent :: Int,
      positionColumn :: Int,
      positionLine :: Int
    }

instance Show Position where
    show (Position {positionColumn, positionLine}) = "line " ++ show positionLine ++ ", column " ++ show positionColumn

type Scanner a = RWS Source (AllE [LexError] [Token]) Position a

data LexError = LexError
    { lexErrortype :: LexErrorType,
      lexErrorLine :: Int
    }

data LexErrorType = UnexpectedChar Char | UnterminatedString

intoAllE :: Either LexError Token -> AllE [LexError] [Token]
intoAllE = either (AllE . Left . singleton) (AllE . Right . singleton)

instance Show LexErrorType where
    show (UnexpectedChar char) = "Unexpected character `" ++ [char] ++ "`."
    show UnterminatedString = "Unterminated string."

instance Show LexError where
    show LexError {lexErrortype, lexErrorLine} =
        "[line "
            ++ show lexErrorLine
            ++ "] Error: "
            ++ show lexErrortype

tokenize :: String -> Either [LexError] [Token]
tokenize src =
    runAllE . snd $
        evalRWS
            scanTokens
            src
            (Position {positionStart = 0, positionCurrent = 0, positionColumn = 1, positionLine = 1})

scanTokens :: Scanner ()
scanTokens = do
    token <- scanToken
    tell $ intoAllE token
    unless (token |> either (const Prelude.False) (tokenType .> (==) Eof)) scanTokens

scanToken :: Scanner (Either LexError Token)
scanToken = do
    result <- scanToken'
    pos <- get
    pure $ (\(tokenType, col) -> Token {tokenType, tokenSpan = pos {positionColumn = col}}) <$> result

scanToken' :: Scanner (Either LexError (TokenType, Int))
scanToken' = do
    pos <- modify (\pos -> pos {positionStart = positionCurrent pos})
    start <- gets positionStart
    colStart <- gets positionColumn
    c <- advance
    case c of
        Nothing -> pure (Right (Eof, colStart))
        Just c -> case c of
            '(' -> pure (Right (LeftParen, colStart))
            ')' -> pure (Right (RightParen, colStart))
            '{' -> pure (Right (LeftBrace, colStart))
            '}' -> pure (Right (RightBrace, colStart))
            ',' -> pure (Right (Comma, colStart))
            '.' -> pure (Right (Dot, colStart))
            '-' -> pure (Right (Minus, colStart))
            '+' -> pure (Right (Plus, colStart))
            ':' -> pure (Right (Colon, colStart))
            ';' -> pure (Right (Semicolon, colStart))
            '*' -> pure (Right (Star, colStart))
            '!' -> Right . (,colStart) <$> scanIfMatch '=' BangEqual Bang
            '=' -> Right . (,colStart) <$> scanIfMatch '=' EqualEqual Equal
            '<' -> Right . (,colStart) <$> scanIfMatch '=' LessEqual Less
            '>' -> Right . (,colStart) <$> scanIfMatch '=' GreaterEqual Greater
            '/' -> do
                next <- peek
                if next == Just '/'
                    then skipComment >> scanToken'
                    else pure (Right (Slash, colStart))
            ' ' -> scanToken'
            '\r' -> scanToken'
            '\t' -> scanToken'
            '\n' -> do
                modify (\pos -> pos {positionColumn = 1, positionLine = positionLine pos + 1})
                scanToken'
            '"' -> fmap (,colStart) <$> tokenizeStr
            c | isDigit c -> fmap (,colStart) <$> tokenizeNum
            c | isAlpha c -> fmap (,colStart) <$> tokenizeIdent
            c -> pure $ Left LexError {lexErrortype = UnexpectedChar c, lexErrorLine = start}

tokenizeStr :: Scanner (Either LexError TokenType)
tokenizeStr = do
    start <- (+ 1) . positionStart <$> get
    tokenizeStr'
    char <- peek
    if isNothing char
        then pure $ Left LexError {lexErrortype = UnterminatedString, lexErrorLine = start}
        else do
            advance
            input <- ask
            end <- subtract 1 . positionCurrent <$> get
            let substr = input |> drop start .> take (end - start)
            pure $ Right (String substr)

tokenizeStr' :: Scanner ()
tokenizeStr' = do
    char <- peek
    unless (char == Just '"' || isNothing char) $ do
        pos <- get
        when (char == Just '\n') (put $ pos {positionLine = positionLine pos + 1})
        advance
        tokenizeStr'

tokenizeNum :: Scanner (Either LexError TokenType)
tokenizeNum = do
    start <- positionStart <$> get
    tokenizeNum'
    char <- peek
    char1 <- peekN 1
    when (char == Just '.' && maybe Prelude.False isDigit char1) $ do
        advance
        tokenizeNum'
    input <- ask
    end <- positionCurrent <$> get
    let substr = input |> drop start .> take (end - start)
    pure . Right $ case readMaybe substr :: Maybe Int of
        Just int -> Int int
        Nothing -> Double (read substr)

tokenizeNum' :: Scanner ()
tokenizeNum' = do
    char <- peek
    when (maybe Prelude.False isDigit char) $ advance >> tokenizeNum'

tokenizeIdent :: Scanner (Either LexError TokenType)
tokenizeIdent = do
    start <- positionStart <$> get
    tokenizeIdent'
    input <- ask
    end <- positionCurrent <$> get
    let substr = input |> drop start .> take (end - start)
    let token = maybe (Identifier substr) Keyword $ keywordKind substr
    pure $ Right token

tokenizeIdent' :: Scanner ()
tokenizeIdent' = do
    char <- peek
    when (maybe Prelude.False isAlphaNum char) $ advance >> tokenizeIdent'

skipComment :: Scanner ()
skipComment = do
    char <- peek
    unless (char == Just '\n' || isNothing char) (advance >> skipComment)

scanIfMatch :: Char -> TokenType -> TokenType -> Scanner TokenType
scanIfMatch char long short = do
    next <- peek
    if next == Just char then advance >> pure long else pure short

peekN :: Int -> Scanner (Maybe Char)
peekN count = do
    text <- ask
    pos <- (+ count) . positionCurrent <$> get
    pure $ text !? pos

peek :: Scanner (Maybe Char)
peek = peekN 0

advance :: Scanner (Maybe Char)
advance = do
    text <- ask
    pos <- get
    let index = positionCurrent pos
    let column = positionColumn pos
    put $ pos {positionCurrent = index + 1, positionColumn = column + 1}
    pure $ text !? index
