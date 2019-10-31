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
    show (SNil) = "Nil"
    show (SNet a b) = "<" ++ (show a) ++ "--" ++ (show b) ++ ">"
    show (WellFormedList l) = unwords $ map show l 

parseSExpr :: Parser (SExpr)
parseSExpr = 
    do
        parseNet 
        +++ parseCode
        +++ parseNode
        +++ parseNames

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
                
-- @TODO figure this out (needs to start at "name" and end at ")))" probably)
-- extract whole name and net to be broken down even more later
parseNames :: Parser SExpr -- parses (name "Net-(C1-Pad1)")
parseNames = do 
    char '('
    string "name"
    name <- manyTill item (string "\")") 
    return (SAtom name) 

parseNode :: Parser SExpr
parseNode = do   
                char '('
                string "node"
                x <- manyTill item (string "))") -- think about this...
                return (SAtom x)
                        

-- parseConnections :: Parser SExpr
-- parseConnections = do   
--                 char '('
--                 string "name"
--                 name <- manyTill item (string "\")")-- consume name
--                 x <- many parseNode
--                 if length x > 0 
--                     then return (x!!0)
--                 else return SNil 

-- how is this actually working
manyTill :: Parser Char -> Parser String -> Parser String
manyTill p end      = scan
                    where
                        scan  = do{ _ <- end; return "" }
                            +++
                                do{ x <- p; xs <- scan; return ([x] ++ xs) }

-- parseAtom :: Parser SExpr
-- parseAtom = do
--                 result <- many (noneOf "()") 
--                 return (SAtom result) -- lift from Str to SExpr
