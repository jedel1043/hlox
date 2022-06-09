{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Parser (parse, ParseError) where

import Control.Arrow (Arrow (second))
import Control.Category ((>>>))
import Control.Monad (unless, void, when)
import Control.Monad.Signatures (Catch)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (runContT), callCC, evalContT, liftLocal, mapContT)
import Control.Monad.Trans.Except (Except, runExcept)
import qualified Control.Monad.Trans.Except as Ex
import Control.Monad.Trans.RWS.Lazy (RWST (RWST, runRWST), ask, asks, evalRWST, get, gets, liftCatch, local, modify)
import Data.Functor ((<&>))
import Data.List.Extra (snoc, (!?))
import Data.Maybe (isJust)
import Data.Tuple.Extra (first)
import Flow ((.>), (<.), (<|), (|>))
import GHC.Conc (orElse)
import Lib.Ast (Variable (variableLevel))
import qualified Lib.Ast as Ast
import qualified Lib.Lexer as Lex

type Finally m a = m a -> m () -> m a

exTryE :: Monad m => Ex.ExceptT e m a -> Ex.ExceptT e m (Either e a)
exTryE m = Ex.catchE (fmap Right m) (return . Left)

exFinallyE :: Monad m => Ex.ExceptT e m a -> Ex.ExceptT e m () -> Ex.ExceptT e m a
exFinallyE m closer = do
    res <- exTryE m
    closer
    either Ex.throwE return res

liftFinally :: Functor m => Finally m (a, s, w) -> Finally (RWST r w s m) a
liftFinally finallyE m p =
    RWST $ \r s -> runRWST m r s `finallyE` void (runRWST p r s)

type Position = Int
data Flags = Flags {inLoop :: Bool, inFun :: Bool, inClass :: Bool, inInit :: Bool, inSubclass :: Bool}

defaultFlags :: Flags
defaultFlags = Flags {inLoop = False, inFun = False, inClass = False, inInit = False, inSubclass = False}

setInLoop :: Flags -> Flags
setInLoop flags = flags {inLoop = True}

setInFun :: Flags -> Flags
setInFun flags = flags {inFun = True}

setInClass :: Flags -> Flags
setInClass flags = flags {inClass = True}

setInSubclass :: Flags -> Flags
setInSubclass flags = flags {inSubclass = True}

setInInit :: Flags -> Flags
setInInit flags = flags {inInit = True}

unsetInInit :: Flags -> Flags
unsetInInit flags = flags {inInit = False}

type Parser = RWST ([Lex.Token], Flags) () Position (Except ParseError)

throwE :: ParseErrorKind -> Parser a
throwE err = do
    tokSpan <- getLastTokenSpan
    (lift . Ex.throwE) ParseError {parseErrorKind = err, parseErrorSpan = tokSpan}

catchE :: Catch ParseError Parser a
catchE = liftCatch Ex.catchE

finallyE :: Parser a -> Parser () -> Parser a
finallyE = liftFinally exFinallyE

data ParseError = ParseError {parseErrorKind :: ParseErrorKind, parseErrorSpan :: Lex.Position}

instance Show ParseError where
    show (ParseError {..}) = "At " ++ show parseErrorSpan ++ " ... " ++ show parseErrorKind ++ "."

data ParseErrorKind
    = UnclosedParen
    | UnexpectedEof
    | Unimplemented
    | ExpectedExpression
    | ExpectedSemicolon String
    | ExpectedIdentifier String
    | InvalidLValue
    | UnclosedBlock
    | ExpectedBlock
    | InvalidFlow String String
    | UnclosedFunctionCall
    | MaxArgsLimit
    | MissingFunBody String
    | MissingLeftParen String
    | MissingRightParen String
    | InvalidThis
    | InvalidReturnInit
    | SuperWithoutDot
    | SuperWithoutIdent
    | SuperOutsideClass
    | SuperWithoutSuperclass

instance Show ParseErrorKind where
    show UnclosedParen = "Unclosed parenthesis expression"
    show UnexpectedEof = "Got to the end of file with unfinished parsing"
    show Unimplemented = "Unimplemented feature"
    show ExpectedExpression = "Expected expression"
    show (ExpectedSemicolon str) = "Expected ';' after " ++ str ++ ""
    show (ExpectedIdentifier name) =
        "Expected identifier for " ++ name
            ++ " name"
    show InvalidLValue = "Invalid assignment target"
    show UnclosedBlock = "Expected `}` after block"
    show ExpectedBlock = "Expected block"
    show (InvalidFlow control valid) = "Invalid control flow statement `" ++ control ++ "` outside a " ++ valid ++ ""
    show UnclosedFunctionCall = "Expected `)` after function arguments"
    show MaxArgsLimit = "Can't have more than 255 arguments"
    show (MissingFunBody typ) = "Missing body of " ++ typ ++ " declaration"
    show (MissingLeftParen typ) = "Expected `(` after " ++ typ ++ " name"
    show (MissingRightParen typ) = "Expected `)` after " ++ typ ++ " parameters"
    show InvalidThis = "Can't use `this` outside of a class"
    show InvalidReturnInit = "Can't return a value from an initializer"
    show SuperWithoutDot = "Expected `.` after `super`."
    show SuperWithoutIdent = "Expected superclass method name."
    show SuperOutsideClass = "Can't use 'super' outside of a class."
    show SuperWithoutSuperclass = "Can't use 'super' in a class with no superclass."

parse :: [Lex.Token] -> Either ParseError [Ast.Stmt]
parse tokens = runExcept $ fst <$> evalRWST program (tokens, defaultFlags) 0

program :: Parser [Ast.Stmt]
program = program' []
  where
    program' :: [Ast.Stmt] -> Parser [Ast.Stmt]
    program' stmts = do
        atEnd <- atEnd
        if atEnd
            then pure stmts
            else declaration >>= (snoc stmts .> program')

declaration :: Parser Ast.Stmt
declaration = finallyE (evalContT $ callCC cc) sync
  where
    cc ret = do
        var <- lift $ matchAdvance (Lex.Keyword Lex.Var)
        when var (lift varDeclaration >>= ret)

        liftLocal ask local (second setInClass) $ do
            clas <- lift $ matchAdvance (Lex.Keyword Lex.Class)
            when clas (lift classDeclaration >>= ret)

        fun <- lift $ matchAdvance (Lex.Keyword Lex.Fun)
        when fun (lift (Ast.FunStmt <$> funExpr Ast.FunKind) >>= ret)

        lift statement

ident :: Lex.TokenType -> Maybe String
ident (Lex.Identifier str) = Just str
ident _ = Nothing

classDeclaration :: Parser Ast.Stmt
classDeclaration = do
    className <- consume ident (ExpectedIdentifier "class")

    less <- matchAdvance Lex.Less
    super <-
        if less
            then Just <$> consume ident (ExpectedIdentifier "superclass")
            else pure Nothing

    let subclass = if isJust super then second setInSubclass else id

    local subclass $ do
        expect Lex.LeftBrace (MissingFunBody "class")
        classMethods <- classMethods []
        expect Lex.RightBrace UnclosedBlock
        pure $
            Ast.ClassStmt
                { Ast.className = className,
                  Ast.classSuper = flip Ast.Variable Nothing <$> super,
                  Ast.classMethods = classMethods
                }
  where
    classMethods :: [Ast.Function Ast.Stmt] -> Parser [Ast.Function Ast.Stmt]
    classMethods methods = do
        tok <- peekType
        if tok /= Lex.RightBrace && tok /= Lex.Eof
            then funExpr Ast.MethodKind >>= classMethods . snoc methods
            else pure methods

varDeclaration :: Parser Ast.Stmt
varDeclaration = do
    varName <- consume ident (ExpectedIdentifier "variable")
    init <- matchAdvance Lex.Equal
    varInit <- if init then Just <$> expression else pure Nothing
    expect Lex.Semicolon $ ExpectedSemicolon "variable declaration"
    pure $ Ast.VarStmt {Ast.varName, Ast.varInit}

funExpr :: Ast.FunType -> Parser (Ast.Function Ast.Stmt)
funExpr typ = local (second unsetInInit) $ do
    lParen <- matchAdvance Lex.LeftParen
    funName <-
        if lParen
            then pure Nothing
            else do
                name <- consume ident (ExpectedIdentifier $ show typ)
                expect Lex.LeftParen (MissingLeftParen (show typ))
                pure (Just name)

    inClass <- asks (inClass . snd)
    let init = if inClass && funName == Just "init" then second setInInit else id

    local init $ do
        rParen <- matchAdvance Lex.RightParen
        params <-
            if rParen
                then pure []
                else do
                    p <- funParams []
                    expect Lex.RightParen (MissingRightParen (show typ))
                    pure p
        expect Lex.LeftBrace (MissingFunBody $ show typ)
        local (second setInFun) (Ast.Function funName params <$> blockStatements)
  where
    funParams :: [String] -> Parser [String]
    funParams params = do
        when (length params >= 255) (throwE MaxArgsLimit)
        param <- consume ident (ExpectedIdentifier "parameter")
        let newParams = params `snoc` param
        comma <- matchAdvance Lex.Comma
        if comma
            then funParams newParams
            else pure newParams

statement :: Parser Ast.Stmt
statement = evalContT $ callCC cc
  where
    cc ret = do
        leftBrace <- lift $ matchAdvance Lex.LeftBrace
        when leftBrace (lift blockStatement >>= ret)

        ifStmt <- lift $ matchAdvance (Lex.Keyword Lex.If)
        when ifStmt (lift ifStatement >>= ret)

        print <- lift $ matchAdvance (Lex.Keyword Lex.Print)
        when print (lift printStatement >>= ret)

        inFun <- lift $ asks (inFun . snd)

        retKW <- lift $ matchAdvance (Lex.Keyword Lex.Return)
        when retKW $ do
            if inFun
                then lift returnStatement >>= ret
                else (lift . throwE) (InvalidFlow "return" "function")

        liftLocal ask local (second setInLoop) $ do
            loop <- lift $ matchAdvance (Lex.Keyword Lex.Loop)
            when loop (lift loopStatement >>= ret)

            while <- lift $ matchAdvance (Lex.Keyword Lex.While)
            when while (lift whileStatement >>= ret)

            for <- lift $ matchAdvance (Lex.Keyword Lex.For)
            when for (lift forStatement >>= ret)

        inLoop <- lift $ asks (inLoop . snd)

        break <- lift $ matchAdvance (Lex.Keyword Lex.Break)
        when break $ do
            if inLoop
                then do
                    a <- lift $ expect Lex.Semicolon (ExpectedSemicolon "break statement")
                    ret Ast.BreakStmt
                else (lift . throwE) (InvalidFlow "break" "loop")

        continue <- lift $ matchAdvance (Lex.Keyword Lex.Continue)
        when continue $ do
            if inLoop
                then do
                    a <- lift $ expect Lex.Semicolon (ExpectedSemicolon "continue statement")
                    ret Ast.ContinueStmt
                else (lift . throwE) (InvalidFlow "continue" "loop")

        lift expressionStatement

ifStatement :: Parser Ast.Stmt
ifStatement = do
    ifCond <- expression
    expect Lex.LeftBrace ExpectedBlock
    ifTrueBranch <- blockStatements

    elseExpr <- matchAdvance (Lex.Keyword Lex.Else)
    ifFalseBranch <-
        if elseExpr
            then do
                expect Lex.LeftBrace ExpectedBlock
                Just <$> blockStatements
            else pure Nothing
    pure (Ast.IfStmt ifCond ifTrueBranch ifFalseBranch)

printStatement :: Parser Ast.Stmt
printStatement = do
    expr <- expression
    expect Lex.Semicolon $ ExpectedSemicolon "value"
    pure $ Ast.PrintStmt expr

returnStatement :: Parser Ast.Stmt
returnStatement = do
    semicolon <- matchAdvance Lex.Semicolon
    if semicolon
        then pure (Ast.ReturnStmt (Ast.Literal Ast.Nil))
        else do
            inInit <- asks (inInit . snd)
            when inInit (throwE InvalidReturnInit)
            expr <- expression
            expect Lex.Semicolon $ ExpectedSemicolon "return value"
            pure (Ast.ReturnStmt expr)

loopStatement :: Parser Ast.Stmt
loopStatement = do
    expect Lex.LeftBrace ExpectedBlock
    Ast.LoopStmt <$> blockStatements

whileStatement :: Parser Ast.Stmt
whileStatement = do
    whileCond <- expression
    expect Lex.LeftBrace ExpectedBlock
    desugarWhile whileCond <$> blockStatements

desugarWhile :: Ast.Expr -> [Ast.Stmt] -> Ast.Stmt
desugarWhile cond body =
    let loopCont = Ast.IfStmt (Ast.Unary Ast.Not cond) [Ast.BreakStmt] Nothing
     in Ast.LoopStmt (loopCont : body)

forStatement :: Parser Ast.Stmt
forStatement = do
    initializer <- (evalContT . callCC) $ \ret -> do
        semicolon <- lift $ matchAdvance Lex.Semicolon
        when semicolon (ret Nothing)

        var <- lift $ matchAdvance (Lex.Keyword Lex.Var)
        when var (lift varDeclaration >>= ret . Just)

        Just <$> lift expressionStatement

    condition <- do
        semicolon <- matchAdvance Lex.Semicolon
        if semicolon
            then pure Nothing
            else do
                condition <- expression
                expect Lex.Semicolon $ ExpectedSemicolon "loop condition"
                pure (Just condition)

    increment <- do
        rbrace <- matchAdvance Lex.LeftBrace
        if rbrace
            then pure Nothing
            else do
                increment <- expression
                expect Lex.LeftBrace ExpectedBlock
                pure (Just increment)

    body <- blockStatements

    let finalBody = case increment of
            Just inc -> body `snoc` Ast.ExprStmt inc
            Nothing -> body

    let loop = case condition of
            Just cond -> desugarWhile cond finalBody
            Nothing -> Ast.LoopStmt finalBody

    let finalLoop = case initializer of
            Just init -> Ast.BlockStmt [init, loop]
            Nothing -> loop

    pure finalLoop

blockStatement :: Parser Ast.Stmt
blockStatement = Ast.BlockStmt <$> blockStatements

blockStatements :: Parser [Ast.Stmt]
blockStatements = blockStatements' []
  where
    blockStatements' :: [Ast.Stmt] -> Parser [Ast.Stmt]
    blockStatements' stmts = do
        tok <- peekType
        if tok /= Lex.RightBrace && tok /= Lex.Eof
            then declaration >>= (snoc stmts .> blockStatements')
            else expect Lex.RightBrace UnclosedBlock >> pure stmts

expressionStatement :: Parser Ast.Stmt
expressionStatement = do
    expr <- expression
    expect Lex.Semicolon $ ExpectedSemicolon "expression"
    pure $ Ast.ExprStmt expr

expression :: Parser Ast.Expr
expression = assignment

assignment :: Parser Ast.Expr
assignment = do
    expr <- orE
    equal <- matchAdvance Lex.Equal
    if equal
        then do
            value <- assignment
            case expr of
                Ast.Var var -> pure (Ast.Assign var value)
                Ast.Get {..} ->
                    pure
                        ( Ast.Set
                            { Ast.setObject = getObject,
                              Ast.setName = getName,
                              Ast.setValue = value
                            }
                        )
                _ -> throwE InvalidLValue
        else pure expr

orE :: Parser Ast.Expr
orE = andE >>= orE'
  where
    orE' :: Ast.Expr -> Parser Ast.Expr
    orE' expr = do
        or <- matchAdvance (Lex.Keyword Lex.Or)
        if or
            then Ast.Logical Ast.Or expr <$> andE
            else pure expr

andE :: Parser Ast.Expr
andE = equality >>= andE'
  where
    andE' :: Ast.Expr -> Parser Ast.Expr
    andE' expr = do
        and <- matchAdvance (Lex.Keyword Lex.And)
        if and
            then Ast.Logical Ast.And expr <$> equality
            else pure expr

equality :: Parser Ast.Expr
equality = comparison >>= equality'
  where
    equality' :: Ast.Expr -> Parser Ast.Expr
    equality' left = do
        op <- advanceIf equals
        case op of
            Just op -> comparison >>= (Ast.Binary op left .> equality')
            Nothing -> pure left
      where
        equals Lex.EqualEqual = Just Ast.Equal
        equals Lex.BangEqual = Just Ast.NotEqual
        equals _ = Nothing

comparison :: Parser Ast.Expr
comparison = term >>= comparison'
  where
    comparison' :: Ast.Expr -> Parser Ast.Expr
    comparison' left = do
        op <- advanceIf compare
        case op of
            Just op -> term >>= (Ast.Binary op left >>> comparison')
            Nothing -> pure left
      where
        compare Lex.Greater = Just Ast.Greater
        compare Lex.GreaterEqual = Just Ast.GreaterEqual
        compare Lex.Less = Just Ast.Less
        compare Lex.LessEqual = Just Ast.LessEqual
        compare _ = Nothing

term :: Parser Ast.Expr
term = factor >>= term'
  where
    term' :: Ast.Expr -> Parser Ast.Expr
    term' left = do
        op <- advanceIf pm
        case op of
            Just op -> factor >>= (Ast.Binary op left .> term')
            Nothing -> pure left
      where
        pm Lex.Plus = Just Ast.Plus
        pm Lex.Minus = Just Ast.Minus
        pm _ = Nothing

factor :: Parser Ast.Expr
factor = unary >>= factor'
  where
    factor' :: Ast.Expr -> Parser Ast.Expr
    factor' left = do
        op <- advanceIf multdiv
        case op of
            Just op -> unary >>= (Ast.Binary op left .> factor')
            Nothing -> pure left
      where
        multdiv Lex.Star = Just Ast.Times
        multdiv Lex.Slash = Just Ast.Div
        multdiv _ = Nothing

unary :: Parser Ast.Expr
unary = do
    op <- advanceIf bangMinus
    case op of
        Just op -> Ast.Unary op <$> unary
        Nothing -> call
  where
    bangMinus Lex.Bang = Just Ast.Not
    bangMinus Lex.Minus = Just Ast.Negative
    bangMinus _ = Nothing

call :: Parser Ast.Expr
call = primary >>= call'
  where
    call' :: Ast.Expr -> Parser Ast.Expr
    call' callee = do
        leftParen <- matchAdvance Lex.LeftParen
        if leftParen
            then finishCall callee >>= call'
            else do
                dot <- matchAdvance Lex.Dot
                if dot
                    then consume ident (ExpectedIdentifier "property") >>= call' . Ast.Get callee
                    else pure callee

finishCall :: Ast.Expr -> Parser Ast.Expr
finishCall callee = do
    rightParen <- matchAdvance Lex.RightParen
    args <-
        if rightParen
            then pure []
            else do
                args' <- finishCall' []
                expect Lex.RightParen UnclosedFunctionCall
                pure args'
    pure (Ast.Call callee args)
  where
    finishCall' :: [Ast.Expr] -> Parser [Ast.Expr]
    finishCall' args = do
        when (length args >= 255) (throwE MaxArgsLimit)
        arg <- expression
        let args' = args `snoc` arg
        comma <- matchAdvance Lex.Comma
        if comma
            then finishCall' args'
            else pure args'

primary :: Parser Ast.Expr
primary = evalContT $ callCC cc
  where
    cc ret = do
        term <- lift $ advanceIf literal
        maybe (pure ()) (ret . Ast.Literal) term

        inClass <- lift $ asks (inClass . snd)

        super <- lift $ matchAdvance (Lex.Keyword Lex.Super)
        when super $ do
            inSubclass <- lift $ asks (inSubclass . snd)
            case (inClass, inSubclass) of
                (False, _) -> lift $ throwE SuperOutsideClass
                (_, False) -> lift $ throwE SuperWithoutSuperclass
                _ -> do
                    lift $ expect Lex.Dot SuperWithoutDot
                    method <- lift $ consume superMethod SuperWithoutIdent
                    ret $ Ast.Super {Ast.superMethod = method, Ast.superLevel = Nothing}

        this <- lift $ matchAdvance (Lex.Keyword Lex.This)
        case (this, inClass) of
            (True, True) -> ret $ Ast.This Nothing
            (True, False) -> lift $ throwE InvalidThis
            _ -> pure ()

        lift (advanceIf ident >>= maybe fun pure)
    superMethod (Lex.Identifier name) = Just name
    superMethod _ = Nothing

    ident (Lex.Identifier name) = Just $ Ast.Var Ast.Variable {Ast.variableName = name, Ast.variableLevel = Nothing}
    ident _ = Nothing

    literal (Lex.Keyword Lex.True) = Just $ Ast.Boolean True
    literal (Lex.Keyword Lex.False) = Just $ Ast.Boolean False
    literal (Lex.Keyword Lex.Nil) = Just Ast.Nil
    literal (Lex.String str) = Just $ Ast.String str
    literal (Lex.Double dbl) = Just $ Ast.Double dbl
    literal (Lex.Int int) = Just $ Ast.Int int
    literal _ = Nothing

fun :: Parser Ast.Expr
fun = do
    fun <- matchAdvance (Lex.Keyword Lex.Fun)
    if fun
        then local (second setInFun) (Ast.FunExpr <$> funExpr Ast.FunKind)
        else grouping

grouping :: Parser Ast.Expr
grouping = do
    lexeme <- advance
    case lexeme of
        Lex.LeftParen -> do
            expr <- expression
            expect Lex.RightParen UnclosedParen
            pure expr
        Lex.Eof -> throwE UnexpectedEof
        _ -> throwE ExpectedExpression

advanceIf :: (Lex.TokenType -> Maybe a) -> Parser (Maybe a)
advanceIf f = do
    res <- f <$> peekType
    when (isJust res) $ void advance
    pure res

peek :: (Lex.Token -> a) -> Parser a
peek = flip fmap (gets (flip (!!)) <*> asks fst)

peekType :: Parser Lex.TokenType
peekType = peek Lex.tokenType

getLastTokenSpan :: Parser Lex.Position
getLastTokenSpan = previous >>= maybe (peek Lex.tokenSpan) (pure . Lex.tokenSpan)

advance :: Parser Lex.TokenType
advance = do
    token <- peekType
    atEnd <- atEnd
    unless atEnd $ modify (+ 1)
    pure token

previous :: Parser (Maybe Lex.Token)
previous = do
    tokens <- asks fst
    pos <- get

    if pos == 0
        then pure Nothing
        else pure . Just $ tokens !! (pos - 1)

atEnd :: Parser Bool
atEnd = (== Lex.Eof) <$> peekType

consume :: (Lex.TokenType -> Maybe a) -> ParseErrorKind -> Parser a
consume f err = do
    token <- advance
    case f token of
        Just res -> pure res
        Nothing -> throwE err

expect :: Lex.TokenType -> ParseErrorKind -> Parser ()
expect typ err = do
    token <- advance
    if token == typ then pure () else throwE err

matchAdvance :: Lex.TokenType -> Parser Bool
matchAdvance typ = do
    token <- peekType
    if token == typ
        then advance >> pure True
        else pure False

sync :: Parser ()
sync = advance >> sync'
  where
    sync' :: Parser ()
    sync' = do
        atEnd <- atEnd
        unless atEnd $ do
            prev <- previous
            curr <- peekType
            unless (fmap Lex.tokenType prev == Just Lex.Semicolon || Lex.isKeyword curr) $ advance >> sync'
