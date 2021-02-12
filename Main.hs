{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
import Control.Applicative
import Control.Monad hiding (function)
import Control.Monad.Fail

import LLVM.AST hiding (function)
import LLVM.AST.Type hiding (void)
import LLVM.IRBuilder.Instruction       
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant

import qualified LLVM.CodeGenOpt          as CodeGenOpt
import qualified LLVM.CodeModel           as CodeModel
import qualified LLVM.Relocation          as Reloc
import           Data.IORef
import           Foreign.Ptr
import           LLVM.Target
import           LLVM.Context
import qualified LLVM.Module              as M
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.Internal.ObjectFile

foreign import ccall "dynamic" mkFun :: FunPtr (IO Int) -> (IO Int)

run :: FunPtr a -> IO Int
run fn = mkFun (castFunPtr fn :: FunPtr (IO Int))

-- <expr>  ::= <term> "+" <expr>
--          |  <term>
--
-- <term> ::= <factor> "*" <term>
--         |  <factor>
--
-- <factor> ::= "(" <expr> ")"
--           |  <const>
--
-- <const> ::= integer

data Operator
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Show, Eq)


data Token
    = TokInt Int
    | TokOp  Operator
    | TokLParen
    | TokRParen
    | TokEnd
    deriving (Show, Eq)


data Expr
    = Expr Expr Operator Expr
    | Term Expr Operator Expr
    | Fact Int
    deriving (Show, Eq)


newtype Parser a 
    = Parser { getParser :: ([Token] -> [(a, [Token])]) }

instance Functor Parser where
    fmap f p = Parser $ \toks -> [ (f x, toks') | (x, toks') <- (getParser p) toks ]

instance Applicative Parser where
    pure x    = Parser $ \toks -> [(x, toks)]
    pf <*> px = Parser $ \toks ->
        concat [ (getParser $ fmap f px) toks' | (f, toks') <- (getParser pf) toks ]
 
instance Monad Parser where
    return  = pure
    p >>= f = Parser $ \toks ->
        concat [ (getParser $ f x) toks' | (x, toks') <- (getParser p) toks ]

instance Alternative Parser where
    empty     = Parser $ \toks -> []
    pa <|> pb = Parser $ \toks -> (getParser pa) toks ++ (getParser pb) toks
    

instance MonadFail Parser where
    fail = error


lexToken :: String -> (Token, String) 
lexToken s = case s of
    '+':ss          -> (TokOp Plus, ss)
    '-':ss          -> (TokOp Minus, ss)
    '*':ss          -> (TokOp Times, ss)
    '/':ss          -> (TokOp Divide, ss)
    '(':ss          -> (TokLParen, ss)
    ')':ss          -> (TokRParen, ss)
    c:_ | isDigit c -> (TokInt (read $ takeWhile isDigit s), dropWhile isDigit s)
    ""              -> (TokEnd, "")
    ' ':ss          -> lexToken ss
    _               -> error ("lex error at: " ++ s)


lexTokens :: String -> [Token]
lexTokens s = case lexToken s of
    (tok, "") -> [tok]
    (tok, s)  -> (tok:lexTokens s)


parseToken :: Token -> Parser Token
parseToken tok = Parser $ \toks -> case toks of
    (t:ts) | t == tok -> [(t, ts)]
    _                 -> []


parseInt :: Parser Int
parseInt = Parser $ \toks -> case toks of
    (TokInt n:ts) -> [(n, ts)]
    _             -> []


parseFactor :: Parser Expr
parseFactor = fmap Fact parseInt <|> do
    parseToken TokLParen
    expr <- parseExpr
    parseToken TokRParen
    return expr


parseTerm :: Parser Expr
parseTerm = parseFactor <|> do
    fact <- parseFactor
    TokOp op <- parseToken (TokOp Times) <|> parseToken (TokOp Divide)
    term <- parseTerm
    return (Term fact op term)


parseExpr :: Parser Expr
parseExpr = parseTerm <|> do
    term <- parseTerm
    TokOp op <- parseToken (TokOp Plus) <|> parseToken (TokOp Minus)
    expr <- parseExpr
    return (Expr term op expr)


parse :: [Token] -> Expr
parse toks = case filter (null . snd) (getParser parseExpr $ toks) of
    []        -> error "can't parse"
    [(e, [])] -> e
    _         -> error "more than one parse"


compile :: Expr -> ModuleBuilder ()
compile expr =
    void $ function (mkName "main") [] i64 $ \_ ->
        ret =<< compileExpr expr


compileExpr :: Monad m => Expr -> IRBuilderT m Operand
compileExpr e = case e of
    Expr term Plus expr   -> join $ liftM2 add (compileExpr term) (compileExpr expr)
    Expr term Minus expr  -> join $ liftM2 sub (compileExpr term) (compileExpr expr)
    Term fact Times term  -> join $ liftM2 mul (compileExpr fact) (compileExpr term)
    Term fact Divide term -> join $ liftM2 sdiv (compileExpr fact) (compileExpr term)
    Fact n                -> return $ int64 (fromIntegral n)




withSession :: (Context -> ExecutionSession -> IRCompileLayer ObjectLinkingLayer -> IO ()) -> IO ()
withSession f = do
    resolvers <- newIORef []
    withContext $ \ctx -> 
        withExecutionSession $ \es ->
            withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.None $ \tm -> do
                withObjectLinkingLayer es (\_ -> fmap head $ readIORef resolvers) $ \oll ->
                    withIRCompileLayer oll tm $ \cl ->
                        withSymbolResolver es (myResolver cl) $ \psr -> do
                            writeIORef resolvers [psr]
                            f ctx es cl
                                
    where
        myResolver :: IRCompileLayer ObjectLinkingLayer -> SymbolResolver
        myResolver cl = SymbolResolver $ \mangled -> do
            symbol <- findSymbol cl mangled False
            case symbol of
                Right _ -> return symbol
                Left _  -> error ("symbol resolver error: " ++ show symbol)


jitAndRunMain :: [Definition] -> ExecutionSession -> Context -> IRCompileLayer ObjectLinkingLayer -> IO ()
jitAndRunMain defs es ctx cl = do
    let astmod = defaultModule { moduleDefinitions = defs }
    withModuleKey es $ \modKey ->
        M.withModuleFromAST ctx astmod $ \mod -> do
            addModule cl modKey mod
            mangled <- mangleSymbol cl "main"
            Right (JITSymbol fn _) <- findSymbolIn cl modKey mangled False
            run $ castPtrToFunPtr (wordPtrToPtr fn)
            removeModule cl modKey


main :: IO ()
main =
    withSession $ \ctx es cl -> do
        line <- getLine
        let expr = parse (lexTokens line)
        let defs = execModuleBuilder emptyModuleBuilder (compile expr)
        putStrLn (show defs)


