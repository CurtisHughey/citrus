module Main where

import System.IO
import Control.Monad
import Control.Applicative((<*))
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

main = putStrLn "Hello, World!"

-- The operators will just be for primitive data types - the same will have to be defined within the language as interfaces for objects

-- Unary Operations
data UnOp = BNot  -- Boolean not
          | ANot  -- Arithmetic not
          | Pos
          | Neg
          deriving (Eq, Show) 

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Shr
           | Shl
           | BAnd  -- Boolean and, or (could add in xor)
           | BOr 
           | AAnd  -- Arithmetic and, xor, or
           | AXor
           | AOr
           | Asn
           | Eq
           | Neq
           | Grtr
           | Less
           | Gte
           | Lte 
           deriving (Eq, Show)    

data Expr = IntConst Integer   -- Think at this stage I want to allow it to be arbitrary precision, will enforce later?
          | Var String
          | FloatConst Double
          | CharConst Char
          | Unary UnOp Expr
          | Binary BinOp Expr Expr
          deriving (Eq, Show)            
---------

--data Var = Var String  -- Eh
--         deriving (Eq, Show)

-- Gonna need separate declare??
data Stmt = Assign Expr Expr  -- First Expr can only be a Var, not sure best way to enforce ^^^
          | Semi Stmt Stmt
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | While Expr Stmt
          | DoWhile Stmt Expr
          | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) Stmt 
          | Return Expr
          | Skip
          deriving (Eq, Show)


languageDef =
	emptyDef   { Token.commentStart    = "/*"
             , Token.commentEnd      = "*/"
             , Token.commentLine     = "//"
             , Token.nestedComments  = True
             , Token.identStart      = lower <|> char '_'   -- I'm a jerk and require that variables can't begin with upper case
             , Token.identLetter     = alphaNum <|> char '_'
             , Token.reservedNames   = [ "if"
                                       , "else"
                                       , "return"
                                       , "while"
                                       , "do"
                                       , "for"
                                       , "break"
                                       , "skip"  -- continue
                                       , "func"
                                       , "class"
                                       , "new"
                                       , "old"
                                       , "interface"
                                       , "public"
                                       , "private"
                                       , "const"
                                       , "func"
                                       , "volatile"  -- might need^^^
                                       , "null"
                                       , "typedef"  -- Probably going to need variadic...
                                       , "import"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "="  -- Need opStart/opLetter?  Missing any?
                                       , "&", "|", "^", "!", "~"
                                  	   , ">>", "<<"
                                       , "<", ">", "<=", ">="                            
                                       , "&&", "||", "!"  -- Need wayyy more: http://www.tutorialspoint.com/cprogramming/c_operators.htm
                                       ]
             , caseSensitive         = True
             }


lexer = Token.makeTokenParser languageDef

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
semi          = Token.semi          lexer  -- ? do i want?
parens        = Token.parens        lexer
brackets      = Token.brackets      lexer
braces        = Token.braces        lexer
comma         = Token.comma         lexer
integer       = Token.integer       lexer
float         = Token.float         lexer
charLiteral   = Token.charLiteral   lexer 
stringLiteral = Token.stringLiteral lexer
whiteSpace    = Token.whiteSpace    lexer

reservedOp' name = try (string name >> whiteSpace)

-- Copied from http://en.cppreference.com/w/c/language/operator_precedence where relevant
operators = [
                [ Prefix (reservedOp' "~"  >> return (Unary ANot))
                , Prefix (reservedOp' "!"  >> return (Unary BNot)) ]

              , [ Prefix (reservedOp' "+"  >> return (Unary Pos))
                , Prefix (reservedOp' "-"  >> return (Unary Neg)) ]

              , [ Infix (reservedOp' "*"  >> return (Binary Mul)) AssocLeft
                , Infix (reservedOp' "/"  >> return (Binary Div)) AssocLeft
                , Infix (reservedOp' "%"  >> return (Binary Mod)) AssocLeft ]

              , [ Infix (reservedOp' "+"  >> return (Binary Add)) AssocLeft
                , Infix (reservedOp' "-"  >> return (Binary Sub)) AssocLeft ]

              , [ Infix (reservedOp' ">>"  >> return (Binary Shr)) AssocLeft
                , Infix (reservedOp' "<<"  >> return (Binary Shl)) AssocLeft ]

              , [ Infix (reservedOp' "<"  >> return (Binary Less)) AssocLeft
                , Infix (reservedOp' ">"  >> return (Binary Grtr)) AssocLeft
                , Infix (reservedOp' "<="  >> return (Binary Gte)) AssocLeft
                , Infix (reservedOp' ">="  >> return (Binary Lte)) AssocLeft ]

              , [ Infix (reservedOp' "=="  >> return (Binary Eq)) AssocLeft  -- Maybe right ^^^
                , Infix (reservedOp' "!="  >> return (Binary Neq)) AssocLeft ]

              , [ Infix (reservedOp' "&"  >> return (Binary AAnd)) AssocLeft ]

              , [ Infix (reservedOp' "&&"  >> return (Binary BAnd)) AssocLeft ]

              , [ Infix (reservedOp' "||"  >> return (Binary BOr)) AssocLeft ]

              , [ Infix (reservedOp' "^"  >> return (Binary AXor)) AssocLeft ]

              , [ Infix (reservedOp' "|"  >> return (Binary AOr)) AssocLeft ]

              --, [ Infix (reservedOp "="  >>  return (Binary Asn)) AssocLeft ]
            ]

expression :: Parser Expr
expression = buildExpressionParser operators term


term :: Parser Expr
term =  parens expression
    <|> liftM Var identifier
    <|> liftM IntConst integer
    <|> liftM FloatConst float
     -- <|> liftM CharConst char  


statement :: Parser Stmt
statement =  parens statement
         <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt 
sequenceOfStmt = do 
    list <- endBy1 statement' (oneOf "\n;")  -- Would prefer to apply the newline or semi parsers ^^^
    return $ foldr1 Semi list

statement' :: Parser Stmt
statement' =  try assignStmt
          <|> try ifElseStmt  -- Have to try this first, otherwise a full if statement would be parsed
          <|> ifStmt  -- should switch, or at least use try (ambiguous start with assignStmt)
          <|> whileStmt
          <|> doWhileStmt
          <|> forStmt

-- We treat assignment as a statement to avoid =/== errors in conditionals
assignStmt :: Parser Stmt
assignStmt = do
    varName <- identifier
    reservedOp' "="
    value <- expression
    return $ Assign (Var varName) value

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- parens expression
    stmt <- braces statement
    return $ If cond stmt

ifElseStmt :: Parser Stmt
ifElseStmt = do
    reserved "if"
    cond <- parens expression
    passStmt <- braces statement
    reserved "else"
    failStmt <- braces statement
    return $ IfElse cond passStmt failStmt

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- parens expression
    stmt <- braces statement
    return $ If cond stmt


doWhileStmt :: Parser Stmt
doWhileStmt = do
    reserved "do"
    stmt <- braces statement
    reserved "while"
    cond <- parens expression
    semi  -- need ; after while statement, i guess
    return $ DoWhile stmt cond

forStmt :: Parser Stmt
forStmt = do
    reserved "for"
    char '('
    initial <- optionMaybe assignStmt
    semi
    cond <- optionMaybe expression
    semi
    inc <- optionMaybe statement'  -- statement' because it doesn't require semicolon at end.  Could make an argument for assignStmt, but what if you wanted to update with function?   
    char ')'
    stmt <- braces statement
    return $ For initial cond inc stmt
 
returnStmt :: Parser Stmt
returnStmt = do
    reserved "return"
    expr <- expression
    return $ Return expr

skipStmt :: Parser Stmt
skipStmt = do
    reserved "skip"
    return Skip


parser :: Parser Stmt
parser = whiteSpace >> statement <* eof

parseString :: String -> Stmt
parseString str = 
    case parse parser "" str of
        Left e -> error $ show e
        Right r -> r
