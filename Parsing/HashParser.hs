-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs.

module HashParser (parseProgram, removeComment, removeComments) where

import Control.Applicative ((<*), many, (<$>), (*>), (<*>), (<|>), (<$), pure)
import Control.Monad (void)
import Data.Char
import Expressions
import Text.Parsec.Char (oneOf, digit, anyChar, char, satisfy, letter, noneOf)
import Text.Parsec.Combinator (many1, choice, chainl1, sepBy, between, eof,
                              optionMaybe, sepEndBy, sepBy1)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, try)


------------------------ Common used functions --------------------------------

-- Parses some number of whitespaces (zero or more).
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- Wrapper used for parsing without trailing whitespaces.
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Parses a given character and ignores it.
symbol :: Char -> Parser ()
symbol c = void $ char c

-- Parses a given keyword and ignores it.
keyword :: String -> Parser ()
keyword s = lexeme $ parseKey' s
  where parseKey' [x] = symbol x
        parseKey' (x:xs) = symbol x *> parseKey' xs


-------------------- Bottom-level expression parsing --------------------------

-- Parses an expression.
parseExpr :: Parser Expr
parseExpr = parseStr <|> parseVar

-- Parses escaped character inside a string.
escape :: Parser String
escape = (\x y -> [x,y]) <$> char '\\' <*> oneOf "\\\"0nrvtbf"

-- Parses non-escaped character inside a string with quotes.
nonEscape1 :: Parser Char
nonEscape1 = noneOf "\\\"\0\n\r\v\t\b\f"

-- Parses a character of the string in quotes, considering escaped characters.
charStr1 :: Parser String
charStr1 = return <$> nonEscape1 <|> escape

-- Parses a string with qoutes.
string1 :: Parser String
string1 = lexeme $ concat <$> (symbol '"' *> many charStr1 <* symbol '"')

-- Parses non-escaped character inside a string without quotes.
nonEscape2 :: Parser Char
nonEscape2 = satisfy (\x -> isLetter x || isDigit x || elem x "_-/*+?.")

-- Parses a character of the string without quotes,
-- considering escaped characters.
charStr2 :: Parser String
charStr2 = return <$> nonEscape2 <|> escape

-- Parses a string without quotes.
string2 :: Parser String
string2 = lexeme $ concat <$> many1 charStr2

-- Parses a string expression.
parseStr :: Parser Expr
parseStr = Str <$> (try string1 <|> string2)

-- Parses a variable character.
charVar :: Parser Char
charVar = satisfy (\x -> isLetter x || isDigit x || x == '_')

-- Parses a variable.
variable :: Parser String
variable = lexeme $ symbol '$' *> many1 charVar

-- Parses a variable expression.
parseVar :: Parser Expr
parseVar = Var <$> variable


--------------------- Comparison expression parsing ---------------------------

-- Parses a comparison expression.
parseComp :: Parser Comp
parseComp = choice $ map try [parseCEQ, parseCNE, parseCGE, parseCGT,
                             parseCLE, parseCLT, parseCLI]

-- Parses a given binary operator with a given keyword.
parseBinOp :: (Expr -> Expr -> Comp) -> String -> Parser Comp
parseBinOp op k = (op <$> parseExpr) <*> (keyword k *> parseExpr)

-- Parses a CEQ expression.
parseCEQ :: Parser Comp
parseCEQ = parseBinOp CEQ "=="

-- Parses a CNE expression.
parseCNE :: Parser Comp
parseCNE = parseBinOp CNE "/="

-- Parses a CGE expression.
parseCGE :: Parser Comp
parseCGE = parseBinOp CGE ">="

-- Parses a CGT expression.
parseCGT :: Parser Comp
parseCGT = parseBinOp CGT ">"

-- Parses a CLE expression.
parseCLE :: Parser Comp
parseCLE = parseBinOp CLE "<="

-- Parses a CLT expression.
parseCLT :: Parser Comp
parseCLT = parseBinOp CLT "<"

-- Parses a CLI expression.
parseCLI :: Parser Comp
parseCLI = CLI <$> parseExpr


--------------------------- Predicate parsing ---------------------------------

-- Parses a predicate.
parsePred :: Parser Pred
parsePred = parseOr

-- Parses a terminal predicate.
parsePredTerm :: Parser Pred
parsePredTerm = parseP <|> parseNot <|> parseParens

-- Parses Pred predicate.
parseP :: Parser Pred
parseP = Pred <$> parseComp

-- Parses Not predicate.
parseNot :: Parser Pred
parseNot = Not <$> (symbol '!' *> parsePredTerm)

-- Parses And predicate.
parseAnd :: Parser Pred
parseAnd = chainl1 parsePredTerm $ And <$ keyword "&&"

-- Parses Parens predicate.
parseParens :: Parser Pred
parseParens = Parens <$> (between (keyword "(") (keyword ")") parsePred)

-- Parses Or predicate.
parseOr :: Parser Pred
parseOr = chainl1 parseAnd $ Or <$ keyword "||"


--------------------------- Command parsing ------------------------------------

-- Parses Cmd expression.
parseCmd :: Parser Cmd
parseCmd = Cmd <$> parseExpr <*> many parseExpr


----------------------------- Assign parsing -----------------------------------

-- Parses Assign expression. 
parseAssign :: Parser Assign
parseAssign = (Assign <$> parseAssignVar) <*> (keyword "=" *>
                (try parseAssignPiped <|> parseAssignExpr)) <* keyword ";"

-- Parses assign variable (variable name without starting '$').
parseAssignVar :: Parser Expr
parseAssignVar = Var <$> lexeme (many charVar)

-- Parses assign expression.
parseAssignExpr :: Parser (Either Expr Piped)
parseAssignExpr = Left <$> parseExpr

-- Parses assign piped expression.
parseAssignPiped :: Parser (Either Expr Piped)
parseAssignPiped = Right <$> (keyword "`" *> parsePCmd <* keyword "`")


-------------------------- If-then-else parsing --------------------------------

-- Parses a conditional expression.
parseCond :: Parser Conditional
parseCond = try parseIfElse <|> parseIf

-- Parses if-then expression.
parseIf :: Parser Conditional
parseIf = (If <$> (keyword "if" *> parsePred)) <*>
          (keyword "{" *> many parseTLExpr <* keyword "}")

-- Parses if-then-else expression.
parseIfElse :: Parser Conditional
parseIfElse = (IfElse <$> (keyword "if" *> parsePred)) <*>
              (keyword "{" *> many parseTLExpr <* keyword "}") <*>
              (keyword "else" *> keyword "{" *> many parseTLExpr <* keyword "}")


--------------------------- For loop parsing -----------------------------------

-- Parses For expression.
parseFor :: Parser For
parseFor = (For <$> (keyword "for" *> parseAssignVar)) <*> parseInt <*>
            parseInt <*> (keyword "{" *> many parseTLExpr <* keyword "}")

-- Parses Int.
parseInt :: Parser Int
parseInt = rd <$> lexeme (many1 digit)
  where rd = read :: String -> Int

------------------------ Piped commands parsing --------------------------------

-- Parses Piped expression.
parsePiped :: Parser Piped
parsePiped = (try parsePCmd <|> pure Empty) <* keyword ";"

-- Parses piped commands.
parsePCmd :: Parser Piped
parsePCmd = (try (appending pcmd) <|> try (notAppending pcmd) <|> notOut pcmd)
  where pcmd = Piped <$> parsePCommands <*> optionMaybe parseInDir

-- Parses input directory expression.
parseInDir :: Parser Expr
parseInDir = keyword "<" *> parseExpr

-- Continue parsing Piped with output directory expression with appending.
appending :: Parser (Maybe Expr -> Bool -> Piped) -> Parser Piped
appending p = p <*> (Just <$> (keyword ">>" *> parseExpr)) <*> pure True

-- Continue parsing Piped with output directory expression without appending.
notAppending :: Parser (Maybe Expr -> Bool -> Piped) -> Parser Piped
notAppending p = p <*> (Just <$> (keyword ">" *> parseExpr)) <*> pure False

-- Continue parsing Piped without specifying output directory.
notOut :: Parser (Maybe Expr -> Bool -> Piped) -> Parser Piped
notOut p = p <*> pure Nothing <*> pure False

-- Parses piped commands.
parsePCommands :: Parser [Cmd]
parsePCommands = sepBy1 parseCmd (keyword "|")


------------------- Top level expression parsing -------------------------------

-- Parses top level expression.
parseTLExpr :: Parser TLExpr
parseTLExpr = try parseTLCnd <|> try parseTLPiped <|> try parseTLFor <|>
                parseTLAssign

-- Parses condiditonal top level expression.
parseTLCnd :: Parser TLExpr
parseTLCnd = TLCnd <$> parseCond

-- Parses for loop expression.
parseTLFor :: Parser TLExpr
parseTLFor = TLFor <$> parseFor

-- Parses assign expression.
parseTLAssign :: Parser TLExpr
parseTLAssign = TLAssign <$> parseAssign

-- Parses piped expression.
parseTLPiped :: Parser TLExpr
parseTLPiped = TLPiped <$> parsePiped

--------------------------- Comment remover ------------------------------------

-- Function that removes comments from lines.
removeComments :: String -> String
removeComments = unlines . map removeComment . lines

-- Function that removes comment from the given line.
removeComment :: String -> String
removeComment = takeWhile ((/=) '#')


--------------------------------- Parser ---------------------------------------

-- Main parser function.
-- Takes a string and produce an executable list of TLExpr constructs.
parseProgram :: String -> Either String [TLExpr]
parseProgram s = case parse (many parseTLExpr <* eof) "" s of
                      -- TODO bolji opis
                      Left x -> Left $ "Syntax error\n" ++ show x
                      Right x -> Right x

