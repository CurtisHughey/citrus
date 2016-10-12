module Main where

import System.IO
import Control.Monad
import Control.Applicative((<*>))
import Control.Applicative((<$>))
import Control.Applicative((<*))  -- Can I bundle these lines together?
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Need to add void type (or, if not declared in return type, void by default?)!

main = parseFile "test.ci" >>= putStrLn.show

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
          | ByteConst Char
          | StringLiteral String
          | Unary UnOp Expr
          | Binary BinOp Expr Expr
          | Func String [Expr]  -- Eh, do I want string? ^^^
          deriving (Eq, Show)            
---------

data Stmt = Assign (Maybe VarInfo) Expr  -- Maybe Type is the return type, second is the Asn Expr.  String became a type thingy!
          | Semi Stmt Stmt
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | While Expr Stmt
          | DoWhile Stmt Expr
          | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) Stmt 
          | CallFunc Expr
          | DeclFunc VarType String [(VarInfo, Expr)] Stmt -- First is return type, second is function name.  Stmt array is array of (Type, variable).  Last is statements separated by semicolons
          | Return Expr
          | Break
          | Skip
          deriving (Eq, Show)

data VarInfo = VarInfo Bool (Maybe AccessType) VarType  -- Bool is to indicate const.  Also add in volatile, which is easy.  Could make a default for AccessType, and not require it to be Maybe
             deriving (Eq, Show)

data AccessType = Public
                | Private 
                deriving (Eq, Show)

data VarType = IntType
             | FloatType
             | ByteType
             | VoidType
             | ClassType String   -- Records the name of the class (alphanumeric_, first is capital)
             | FuncType [VarType] VarType  -- Takes a list of types and returns a type.  Could also make the first thing a tuple, but then you would need extra parens...
             -- | Tuple [Type]  -- Also todo.  Also need to add a parametric type
             deriving (Eq, Show)


languageDef =
	emptyDef { Token.commentStart    = "/*"
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
                                       , "skip"
                                       , "func"        -- Hmm, actually not using this.  Worth?  Might allow me to remove a couple of trys...
                                       , "class"       -- Implement
                                       , "new"         -- Implement
                                       , "old"         -- Implement
                                       , "interface"   -- Implement
                                       , "implements"  -- Implement
                                       , "is"          -- Implement
                                       , "public"      -- Implement
                                       , "private"     -- Implement
                                       , "const"       -- Implement
                                       , "func"
                                       , "volatile"    -- Implement
                                       , "import"      -- Implement
                                       , "null"        -- Implement
                                       , "int"
                                       , "float"
                                       , "byte"
                                       , "void"
                                       ]
             , Token.reservedOpNames = ["+", "-", "*", "/", "="  -- Need opStart/opLetter?  Missing any?
                                       , "&", "|", "^", "!", "~"
                                  	   , ">>", "<<"
                                       , "<", ">", "<=", ">="                            
                                       , "&&", "||", "!"  -- Need wayyy more: http://www.tutorialspoint.com/cprogramming/c_operators.htm
                                       , "->"  -- Bundling this in
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
commaSep      = Token.commaSep      lexer
symbol        = Token.symbol        lexer

reservedOp' :: String -> GenParser Char st String
reservedOp' = try.symbol

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
            ]

expression :: Parser Expr
expression = buildExpressionParser operators term


assignExpr :: Parser Expr
assignExpr = do
    name <- identifier <?> "asdfasf"  -- I think I wanna write my own thing that returns an Expr
    reservedOp' "="
    value <- expression  -- Need to also do new object ^^^  
    return $ Binary Asn (Var name) value    -- At the end of the day, might not want

callFuncExpr :: Parser Expr
callFuncExpr = do
    name <- identifier 
    args <- parens $ commaSep expression
    return $ Func name args

term :: Parser Expr
term =  try $ parens term  -- I changed this from expression to term, it makes more sense?^^^
    <|> try assignExpr
    <|> try callFuncExpr
    <|> liftM Var identifier
    <|> liftM FloatConst (try float)  -- Use try in case it's actually integer.  Changed, did I break something? ^^^
    <|> liftM IntConst integer
    <|> liftM ByteConst charLiteral  
    <|> liftM StringLiteral stringLiteral


statement :: Parser Stmt
statement =  parens statement  -- need the parens?
         <|> sequenceOfStmts

sequenceOfStmts :: Parser Stmt 
sequenceOfStmts = do 
    -- list <- endBy1 statement' (newline )  -- Would prefer to apply the newline or semi parsers ^^^.  Actually, I should just do end of lines
    list <- many  statement'  -- Wait, does this actually work???  Allows for stuff like x=2y=3, hmm
    return $ foldr1 Semi list

statement' :: Parser Stmt
statement' =  try assignStmt
          <|> try ifElseStmt  -- Have to try this first, otherwise a full if statement would be parsed
          <|> try ifStmt  -- should switch, or at least use try (ambiguous start with assignStmt)
          <|> try whileStmt
          <|> try doWhileStmt
          <|> try forStmt
          <|> try callFuncStmt
          <|> try returnStmt
          <|> try declFuncStmt
          <|> breakStmt
          <|> skipStmt


-- Not allowing _ right now.  Similar to identifier.  Should emulate lexeme parser
parseClassType :: Parser VarType
parseClassType = do
    first <- upper
    rest <- many alphaNum
    whiteSpace
    return $ ClassType (first:rest)

-- Used when declaring a function variable.  Functions are first-class objects, but we don't allow currying
parseFuncType :: Parser VarType  
parseFuncType = braces $ do { argTypes <- commaSep parseVarType  -- Do I want the parens?  Yes, to allow functions accepting functions
                            ; reservedOp' "->"  -- Stealing a bit of Haskell.  Should I be using reservedOp'?
                            ; returnType <- parseVarType
                            ; return $ FuncType argTypes returnType
                            }

-- I guess this means that classes have to be at least two characters
-- Need to add in const, volatile, private, public.  Probably not static...
parseVarType :: Parser VarType 
parseVarType =  parseClassType
            <|> parseFuncType
            <|> (try $ symbol "int" >> return IntType)  -- Do these need to be trys?  Maybe not...
            <|> (try $ symbol "byte" >> return ByteType)
            <|> (try $ symbol "float" >> return FloatType)

parseTypeWithVoid :: Parser VarType
parseTypeWithVoid =  parseVarType
                 <|> (try $ symbol "void" >> return VoidType)

parseConst :: Parser Bool
parseConst =  (try $ reserved "const" >> return True)
          <|> return False  -- Failure means that we are not const

parseAccessType :: Parser AccessType  -- This is janky.  Could also default to public here...
parseAccessType =  (try $ symbol "public" >> return Public)
               <|> (try $ symbol "private" >> return Private)


        -- liftM $ Public $ reserved "public"   -- Not what I want
        --       <|> liftM $ Private $ reserved "private"

parseVarInfo :: Parser VarInfo
parseVarInfo = do
    constType <- parseConst
    accessType <- optionMaybe $ try parseAccessType  -- We need to do these tries, maybe not?
    varType <- parseVarType
    return $ VarInfo constType accessType varType

-- This might need to be renamed since it isn't necessarily an assignment.  Maybe declarationStmt? ^^
assignStmt :: Parser Stmt  -- Will either be in the form "int x = 1;" or "int x;".  In the former, a binary assignment expression is returned, in the former, just a Var
assignStmt = do
    varInfo <- optionMaybe parseVarInfo 
    assign <- try assignExpr <|> liftM Var (try identifier >> semi)  -- Might not need this try ^^^  .  This allows us to either parse an assignment expression or just a variable declaration
    return $ Assign varInfo assign

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

-- Should try to simplify ^^^
forStmt :: Parser Stmt
forStmt = do
    reserved "for"
    symbol "("
    initial <- optionMaybe assignStmt
    semi
    cond <- optionMaybe expression
    semi
    inc <- optionMaybe statement'  -- statement' because it doesn't require semicolon at end.  Could make an argument for assignStmt, but what if you wanted to update with function?   
    symbol ")"
    stmt <- braces statement
    return $ For initial cond inc stmt
 

callFuncStmt :: Parser Stmt
callFuncStmt = do
    callFunc <- callFuncExpr
    return $ CallFunc callFunc

-- Void not allowed as single argument to function.  By default, empty args means absolutely no args can be accepted in function calls, unlike C
declFuncStmt :: Parser Stmt
declFuncStmt = do
    returnType <- parseTypeWithVoid
    name <- identifier
    args <- parens $ commaSep $ (,) <$> parseVarInfo <*> expression  -- This line is black magic, it parses outside parentheses, and then a list of (type, expression), representing the type and values of arguments, separated by commas
    stmts <- braces sequenceOfStmts  -- Must be at least one statement
    return $ DeclFunc returnType name args stmts

returnStmt :: Parser Stmt
returnStmt = do
    reserved "return"
    expr <- expression
    return $ Return expr

breakStmt :: Parser Stmt
breakStmt = do
    reserved "break"
    return Break

skipStmt :: Parser Stmt
skipStmt = do
    reserved "break"
    return Break

parser :: Parser Stmt
parser = whiteSpace >> statement <* eof

parseString :: String -> Stmt
parseString str = 
    case parse parser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse parser "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r
