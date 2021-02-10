module Main where

import Data.Char

data Token
    = TokInt Int
    | TokPlus
    | TokMinus
    | TokTimes
    | TokDivide
    | TokLParen
    | TokRParen
    deriving (Show, Eq)


lexToken :: String -> (Token, String) 
lexToken s = case s of
    '+':ss          -> (TokPlus, ss)
    '-':ss          -> (TokMinus, ss)
    '*':ss          -> (TokTimes, ss)
    '/':ss          -> (TokDivide, ss)
    '(':ss          -> (TokLParen, ss)
    ')':ss          -> (TokRParen, ss)
    c:_ | isDigit c -> (TokInt (read $ takeWhile isDigit s), dropWhile isDigit s)
    ' ':ss          -> lexToken ss
    _               -> error ("lex error at: " ++ s)


lexTokens :: String -> [Token]
lexTokens s = case lexToken s of
    (tok, "") -> [tok]
    (tok, s)  -> (tok:lexTokens s)


data Expr
    = ExprPlus   Expr Expr
    | ExprTerm   Expr
    | TermTimes  Expr Expr
    | TermFactor Expr
    | FactParen  Expr
    | FactConst  Int
    deriving (Show, Eq)

data Const
    = ConstInt Int
    deriving (Show, Eq)



newtype Parser a 
    = Parser { getParser :: ([Token] -> [(a, [Token])]) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \toks -> map (\(r, ts) -> (f r, ts)) (p toks)

instance Applicative Parser where
    pure x                    = Parser $ \toks -> [(x, toks)]
    (Parser f) <*> (Parser x) = Parser $ \toks -> concat $ map (\(rx, ts) -> map (\(rf, ts') -> (rf rx, ts')) (f ts) ) (x toks)

instance Monad Parser where
    return           = pure
    (Parser a) >>= f = Parser $ \toks -> concat $ map (\(r, ts) -> let (Parser b) = f r in b ts) (a toks)


instance MonadFail Parser where
    fail = error


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


parseToken :: Token -> Parser Token
parseToken tok = Parser $ \toks -> case toks of
    (t:ts) | t == tok -> [(t, ts)]
    _                 -> []


parseInt :: Parser Int
parseInt = Parser $ \toks -> case toks of
    (TokInt n:ts) -> [(n, ts)]
    _             -> []


choice :: Parser a -> Parser a -> Parser a
choice (Parser a) (Parser b) = Parser (\toks -> a toks ++ b toks)


parseFactor :: Parser Expr
parseFactor = choice (fmap FactConst parseInt) $ do
    parseToken TokLParen
    n <- parseInt
    parseToken TokRParen
    return $ FactParen (FactConst n) 


parseTerm :: Parser Expr
parseTerm = choice (fmap TermFactor parseFactor) $ do
    fact <- parseFactor
    parseToken TokTimes
    term <- parseTerm
    return (TermTimes fact term)


parseExpr :: Parser Expr
parseExpr = choice (fmap ExprTerm parseTerm) $ do
    term <- parseTerm
    parseToken TokPlus
    expr <- parseExpr
    return (ExprPlus term expr)


parse :: [Token] -> Expr
parse toks = case filter (null . snd) ((getParser parseExpr) toks) of
    []        -> error "can't parse"
    [(e, [])] -> e
    _         -> error "more than one parse"

main :: IO ()
main = do
    line <- getLine
    putStrLn $ show $ parse (lexTokens line)
