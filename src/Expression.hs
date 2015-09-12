{- |
    Expression describes a NUMBER in BulletML.
    Some examples: 35, 360/16, 0.7 + 0.9*$rand, 180-$rank*20, (2+$1)*0.3
-}
module Expression (
    Expression
  , Param
  , Rank
  , parseExpr
  , evalExpr
) where

import Data.Char (isAlphaNum, isDigit, isSpace)

-- Specifies the parameter
type Param = Double

-- Rand defines a random number between 0 and 1
-- Rank is a number proportional to the game difficulty (from 0 to 1)
type Rank = Double
-- Expression <- Term [+-] Expression
--             | Term
-- Term       <- Factor [*/] Term
--             | Factor
-- Factor     <- IntNum
--             | DoubleNum
--             | Identifier
--             | [+-] Factor
--             | '(' Expression ')'
data Expression = EBinOp SumOp Term Expression | ETerm Term deriving (Show, Eq)
data Term = TBinOp ProdOp Factor Term | TFactor Factor deriving (Show, Eq)
data Factor = FNum Double | FVar Int | FRand | FRank | FUnOp SumOp Factor | FParen Expression deriving (Show, Eq)
data SumOp = SPlus | SMinus deriving (Show, Eq)
data ProdOp = PMult | PDiv deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokRand
           | TokRank
           | TokParam Int
           | TokNum Double
           | TokDot
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | otherwise = error "Unknow operator"

isParamId :: String -> Bool
isParamId [c] = isDigit c && c /= '0'
isParamId _ = False

identifier :: Char -> String -> [Token]
identifier '$' cs =
  let (str, cs') = span isAlphaNum cs
      f s
        | s == "rank" = TokRank
        | s == "rand" = TokRand
        | isParamId s = TokParam (read s)
        | otherwise     = error "Invalid string after $"
  in f str : tokenize cs'
identifier _ _ = error "Identifier must start with a $"

number :: Char -> String -> [Token]
number c cs =
   let (digs, cs') = span (\x -> isDigit x || x == '.') cs in
   TokNum (read (c : digs)) : tokenize cs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | c `elem` "+-*/" = TokOp (operator c) : tokenize cs
    | isDigit c = number c cs
    | c == '$' = identifier c cs
    | isSpace c = tokenize cs
    | c == '(' = TokLParen : tokenize cs
    | c == ')' = TokRParen : tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:_) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts

sumOp :: Operator -> SumOp
sumOp Plus  = SPlus
sumOp Minus = SMinus
sumOp _     = error "Illegal token"

prodOp :: Operator -> ProdOp
prodOp Mult = PMult
prodOp Div  = PDiv
prodOp _    = error "Illegal token"

expression :: [Token] -> (Expression, [Token])
expression toks =
   let (termTree, toks') = term toks
   in
      case lookAhead toks' of
         -- Term [+-] Expression
         (TokOp op) | op `elem` [Plus, Minus] ->
            let (exTree, toks'') = expression (accept toks')
            in (EBinOp (sumOp op) termTree exTree, toks'')
         -- Term
         _ -> (ETerm termTree, toks')

term :: [Token] -> (Term, [Token])
term toks =
   let (facTree, toks') = factor toks
   in
      case lookAhead toks' of
         (TokOp op) | op `elem` [Mult, Div] ->
            let (termTree, toks'') = term (accept toks')
            in (TBinOp (prodOp op) facTree termTree, toks'')
         _ -> (TFactor facTree, toks')

factor :: [Token] -> (Factor, [Token])
factor toks =
   case lookAhead toks of
      (TokNum x)   -> (FNum x, accept toks)
      (TokParam p) -> (FVar p, accept toks)
      TokRank -> (FRank, accept toks)
      TokRand -> (FRand, accept toks)
      (TokOp op) | op `elem` [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (FUnOp (sumOp op) facTree, toks')
      TokLParen ->
         let (expTree, toks') = expression (accept toks)
         in
            if lookAhead toks' /= TokRParen
            then error "Missing right parenthesis"
            else (FParen expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks


tokensToExpr :: [Token] -> Expression
tokensToExpr toks = let (tree, toks') = expression toks
             in
               if null toks'
               then tree
               else error $ "Leftover tokens: " ++ show toks'

{- |
    Parse an expression

    parseExpr "($rank * $2 - 0.0) / - 1.0"
    >>> Expression
-}
parseExpr :: String -> Expression
parseExpr = tokensToExpr . tokenize

evalFact :: Factor -> [Param] -> Rank -> Double
evalFact f ps rank = case f of
  FNum x -> x
  FVar idx -> ps !! idx
  FRank -> rank
  FRand -> 0.0 -- TODO implement random
  FUnOp SPlus  f' ->   evalFact f' ps rank
  FUnOp SMinus f' -> - evalFact f' ps rank
  FParen expr -> evalExpr expr ps rank
evalTerm :: Term -> [Param] -> Rank -> Double
evalTerm t ps rank = case t of
  TBinOp p f' t' -> let op = case p of
                               PMult -> (*)
                               PDiv  -> (/)
                    in evalFact f' ps rank `op` evalTerm t' ps rank
  TFactor f' -> evalFact f' ps rank
evalExpr :: Expression -> [Param] -> Rank -> Double
evalExpr e ps rank = case e of
  EBinOp s t e' -> let op = case s of
                              SPlus -> (+)
                              SMinus  -> (-)
                    in evalTerm t ps rank `op` evalExpr e' ps rank
  ETerm t -> evalTerm t ps rank
