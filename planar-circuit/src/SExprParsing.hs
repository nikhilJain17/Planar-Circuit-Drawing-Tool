module SExprParsing where

import Parsing
import Control.Monad

-- functions specifically for parsing S-expr

-- we are only interested in parsing atoms of strings?
data SExpr = -- @todo make a have constraint of show
    WellFormedList [SExpr] 
    | SAtom String
    | SNil

instance Show SExpr where
    show (SAtom a) = show a
    show (WellFormedList l) = unwords $ map show l 

parseSExpr :: Parser (SExpr)
parseSExpr = 
    do
        parseAtom
        +++ do
                char '('
                x <- parseList
                char ')'
                return x

parseList :: Parser (SExpr)
parseList = liftM WellFormedList (sepby parseSExpr spaces)

-- @TODO
parseNet = undefined
parseNode = undefined

-- do i need this lmoa
-- parseAtom :: Parser 