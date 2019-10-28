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
        parseNet 
        +++ parseCode
        +++ parseNode
        +++ parseConnections

-- @todo delete this?
parseList :: Parser (SExpr)
parseList = liftM WellFormedList (sepby parseSExpr spaces)

-- recursive cases for each piece of info in netlist
parseNet :: Parser SExpr
parseNet = do   
                char '('
                string "net"
                x <- many (noneOf "()")
                return (SAtom x)

parseCode :: Parser SExpr
parseCode = do   
                char '('
                string "code" 
                x <- many (noneOf "(") -- consume whole string i.e. "(code 1)" and terminate
                return (SAtom x)
                
parseNode :: Parser SExpr
parseNode = do   
                char '('
                string "node"
                x <- many (noneOf "()")
                return (SAtom x)              

-- @TODO figure this out 
-- extract whole name and net to be broken down even more later
parseConnections :: Parser SExpr
parseConnections = do   
                char '('
                string "name"
                x <- endBy letter (string ")))") -- just get a bunch of letters
                -- string ")))" -- extract actual i.e. (name "Net-(R3-Pad1)")
                return (SAtom x)



-- parseAtom :: Parser SExpr
-- parseAtom = do
--                 result <- many (noneOf "()") 
--                 return (SAtom result) -- lift from Str to SExpr
