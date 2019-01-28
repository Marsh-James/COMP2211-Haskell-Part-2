-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution

convertLet (App expr1 expr2) = convertApp expr1 expr2
convertLet (Var expr) = convertVar (Var expr)
convertLet (Let ns expr1 expr2)
    | length ns > 1 = LamApp (LamAbs (head(ns)) (convertLet expr2)) (padAbstraction (flipList(tail(ns))) (convertLet expr1))
    | length ns == 1 = LamApp (LamAbs (head(ns)) (convertLet expr2)) (convertLet(expr1))
    | length ns == 0 = LamApp (convertLet expr2) (convertLet expr1)

{- convertApp and convertVar abstracted away for readability -}
convertApp :: Expr -> Expr -> LamExpr
convertApp expr1 expr2 = LamApp (convertLet expr1) (convertLet expr2)

convertVar :: Expr -> LamExpr
convertVar (Var expr) = LamVar expr

{- Adds Abstractions to expressions with multiple arguments -}
padAbstraction :: [Int] -> LamExpr -> LamExpr
padAbstraction ns expr
    | length ns > 1 = padAbstraction (tail(ns)) (LamAbs (head(ns)) expr)
    | length ns == 1 = LamAbs (head(ns)) expr

flipList :: [Int] -> [Int]
flipList = foldl (\l x -> x : l) []

-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint (Let ns expr1 expr2) = "let" ++ (expandList ns) ++ " = " ++ (prettyPrint expr1) ++ " in " ++ (prettyPrint expr2)
prettyPrint (Var expr) = varToString (Var expr)
prettyPrint (App expr1 expr2) = appStr
    where appStr
            | (checkExprType expr2) /= "Var" = (prettyPrint expr1) ++ " (" ++ (prettyPrint expr2) ++ ")"
            | (checkExprType expr1) == "Let" = "(" ++ (prettyPrint expr1) ++ ") " ++ (prettyPrint expr2)
            | (checkExprType expr1) == "Let" && (checkExprType expr2) /= "Var" = "(" ++ (prettyPrint expr1) ++ ") (" ++ (prettyPrint expr2) ++ ")"
            | otherwise = (prettyPrint expr1) ++ " " ++ (prettyPrint expr2)

{- Check type is only called to check input expressions subtype since they're all of actual type: Expr-}
checkExprType :: Expr -> String
checkExprType (Var _) = "Var"
checkExprType (App _ _) = "App"
checkExprType (Let _ _ _) = "Let"

varToString :: Expr -> String
varToString (Var expr) = "x" ++ (show(expr))

expandList :: [Int] -> String
expandList ns
    | length ns > 1 = " " ++ varToString (Var (head(ns))) ++ expandList(tail(ns))
    | length ns == 1 = " " ++ varToString (Var (head(ns)))

-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
-- replace the definition below with your solution
parseLet s
    | parse (appStrParser <|> letStrParser <|> varStrParser) s == [] = Nothing
    | otherwise = Just (fst(head(parse (appStrParser <|> letStrParser <|> varStrParser) s)))

{- Parser checks let expressions with strict format -}
letStrParser :: Parser Expr
letStrParser = do
    _ <- symbol "let"
    intList <- parseIntList
    _ <- symbol "=" 
    expr1 <- appStrParser <|> letStrParser <|> varStrParser
    _ <- symbol "in"
    expr2 <- appStrParser <|> letStrParser <|> varStrParser
    return (Let intList expr1 expr2)

{- Parser recursively checks for continuing app expressions in strict format-}
appStrParser :: Parser Expr
appStrParser = do
    expr1 <- rmBrackets <|> varStrParser
    expr2 <- rmBrackets <|> varStrParser
    exprN <- many (appStrParser <|> varStrParser)
    if length exprN == 0 then return (App expr1 expr2)
    else return (App (App expr1 expr2) (exprN !! 0))

{- Parser checks var expressions with strict format -}
varStrParser :: Parser Expr
varStrParser = do
    expr <- intStrParser
    return (Var expr)

rmBrackets :: Parser Expr
rmBrackets = do
    _ <- space
    _ <- char '('
    expr <- appStrParser <|> letStrParser <|> varStrParser
    _ <- char ')'
    _ <- space
    return expr

{- Nat used rather that digitToInt to cover variables >9 -}
intStrParser :: Parser Int
intStrParser = do
    _ <- char 'x'
    intVar <- nat
    _ <- space
    return (intVar)

{- Parser works recursively input is exhausted generating variable list-}
parseIntList :: Parser [Int]
parseIntList = do
    intList <- many intStrParser
    if (length intList == 0) then return ([]) else do
        _ <- space
        nextList <- parseIntList
        return (intList ++ nextList)

-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds expr limit = (tryMultiLayer limit (leftReducer) expr 0, tryMultiLayer limit (rightReducer) expr 0)

{- leftReducer performs left inner most reductions for all types  in LamExpr-}
leftReducer :: LamExpr -> LamExpr
leftReducer (LamApp expr1@(LamAbs x expr) expr2)
    | expr1 == (leftReducer expr1) = subst expr x expr2
    | otherwise = LamApp (leftReducer expr1) expr2

leftReducer (LamApp expr1 expr2)
    | expr1 == (leftReducer expr1) = LamApp expr1 (leftReducer expr2)
    | otherwise = LamApp (leftReducer expr1) expr2
leftReducer (LamVar x) = LamVar x
leftReducer (LamAbs x expr) = LamAbs x (leftReducer expr)

{- rightReducer performs left inner most reductions for all types  in LamExpr-}
rightReducer :: LamExpr -> LamExpr
rightReducer (LamApp expr1@(LamAbs x expr) expr2)
    | expr2 == (rightReducer expr2) = subst expr x expr2
    | otherwise = LamApp expr1 (rightReducer expr2)
rightReducer (LamApp expr1 expr2)
    | expr2 == (rightReducer expr2 )= LamApp (rightReducer expr1) expr2
    | otherwise = LamApp expr1 (rightReducer expr2)
rightReducer (LamVar x) = LamVar x
rightReducer (LamAbs x expr) = LamAbs x (leftReducer expr)

{- multiLayer keeps track of depth of solution to return -}
tryMultiLayer :: Int -> (LamExpr -> LamExpr) -> LamExpr -> Int -> Maybe Int
tryMultiLayer 0 tup expr layers
    | (canReduce tup expr) = Just (layers)
    | otherwise = Nothing
tryMultiLayer n tup expr layers
    | (canReduce tup expr) = Just (layers)
    | otherwise = tryMultiLayer (n - 1) tup (tup expr) (layers + 1)

canReduce :: (LamExpr -> LamExpr) -> LamExpr -> Bool
canReduce tup expr 
    | expr == (tup expr) = True
    | otherwise = False

fitName :: Int -> Int -> Int
fitName x y = (max x y) + 1

unboundVar :: Int -> LamExpr -> Bool
unboundVar x (LamVar y) = x == y
unboundVar x (LamAbs y e) | x == y = False
unboundVar x (LamAbs y e) | x /= y = unboundVar x e
unboundVar x (LamApp e1 e2) = (unboundVar x e1) || (unboundVar x e2)

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y expr1 | x == y = expr1
subst (LamVar x) y expr1 | x /= y = LamVar x
subst (LamAbs x expr2) y expr1 
    | x /= y && not (unboundVar x expr1) = LamAbs x (subst expr2 y expr1)
subst (LamAbs x expr2) y expr1 
    | x /=y && (unboundVar x expr1) = let x' = fitName x y in
        subst (LamAbs x' (subst expr2 x (LamVar x'))) y expr1
subst (LamAbs x expr2) y expr1 | x == y = LamAbs x expr2
subst (LamApp expr2 expr3) y expr1 = LamApp (subst expr2 y expr1) (subst expr3 y expr1) 

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s
    | parse (parseArith) s == [] = Nothing
    | snd(head(parse (parseArith) s)) /= "" = Nothing
    | otherwise = Just (fst(head(parse (parseArith) s)))

parseArith :: Parser LamExpr
parseArith = do
    expr <- parseValue <|> parseSection
    return expr

parseValue :: Parser LamExpr
parseValue = do
    expr <- parseSectionValue <|> parseNatural
    return expr

parseSectionValue :: Parser LamExpr
parseSectionValue = do
    expr1 <- parseSection
    space
    expr2 <- parseNatural <|> parseNaturalBrackets <|> parseValue
    return (LamApp (expr1) (expr2))

parseNaturalAddition :: Parser LamExpr
parseNaturalAddition = do
    val1 <- getBracketNum <|> getNum
    symbol "+"
    val2 <- getBracketNum <|> getNum
    return (generateDigit(val1 + val2))

parseNaturalBrackets :: Parser LamExpr
parseNaturalBrackets = do
    symbol "("
    val <- nat
    symbol ")"
    return (generateDigit val)

parseNatural :: Parser LamExpr
parseNatural = do
    val <- nat
    return (generateDigit val)

parseSection :: Parser LamExpr
parseSection = do
    symbol "("
    symbol "+"
    val <- nat
    symbol ")"
    return (generateSection val)

generateDigit :: Int -> LamExpr
generateDigit val
    | val > 0 = (LamAbs 1 (LamAbs 2 (encodeDigit val)))
    | otherwise = (LamAbs 1 (LamAbs 2 (LamVar 2)))

{- Loops static code to encode digits -}
encodeDigit :: Int -> LamExpr
encodeDigit val
    | val > 1 = (LamApp (LamVar 1) (encodeDigit (val - 1)))
    | otherwise = (LamApp (LamVar 1) (LamVar 2))

{- Gens succ funciton -}
generateSection :: Int -> LamExpr
generateSection val = (LamApp (generateDigit val) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))))))

{- Helper Functions -}
getBracketNum :: Parser Int
getBracketNum = do
    symbol "("
    val <- nat
    symbol ")"
    return (val)

getNum :: Parser Int
getNum = do
    val <- nat
    return (val)
