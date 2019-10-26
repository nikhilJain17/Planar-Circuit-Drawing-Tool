module SExprParsing where

import Parsing
import Control.Monad

-- functions specifically for parsing S-expr

-- we are only interested in parsing atoms of strings?

data SExpr = -- @todo make a have constraint of show
    WellFormedList [SExpr] 
    | SAtom String
    | SNil
    | SNet String String
    | SNode [SExpr] -- @todo SNodes are lists of SNets

instance Show SExpr where
    show (SAtom a) = show a
    show (WellFormedList l) = unwords $ map show l 

parseSExpr :: Parser (SExpr)
parseSExpr = 
    do
        do
            char '('
            x <- parseList
            char ')'
            return x
    +++ parseAtom


parseList :: Parser (SExpr)
parseList = liftM WellFormedList (sepby parseSExpr spaces)

parseAtom :: Parser SExpr
parseAtom = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return (SAtom x)
                
-- parseAtom :: Parser SExpr
-- parseAtom = do
--                 result <- many (noneOf "()") 
--                 return (SAtom result) -- lift from Str to SExpr

-- @TODO
parseNet = undefined
parseNode = undefined

-- do i need this lmoa
-- parseAtom :: Parser 