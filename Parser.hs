module Parser (parse) where

import           AST
import           Control.Applicative
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta

expr :: Parser Expr
expr = optional comment
       *> (try number <|> try call <|> constant <|> inc <|> parens expr)
       <* optional comment

nat :: Parser Int
nat = convert <$> (highlight H.Number (many (char 'S') <* char '0')) <* whiteSpace
  where convert [] = 0
        convert (_:xs) = 1 + convert xs

number :: Parser Expr
number = Number <$> nat

var :: Parser Name
var = highlight H.Identifier ((:) <$> lower <*> many alphaNum) <* whiteSpace

constant :: Parser Expr
constant = Constant <$> var

call :: Parser Expr
call = Call <$> var <*> parens (sepBy expr comma)

inc :: Parser Expr
inc = Inc <$> highlight H.ReservedIdentifier (char 'S' *> parens expr)

patternParam :: Parser Pattern
patternParam = convert <$> many (char 'S') <*> var
  where convert [] n = Binding n
        convert (_:rest) n = Succ (convert rest n)

functionDefinition :: Parser Definition
functionDefinition = convert <$> var <*>
                     parens (sepBy (FreeParam <$> var
                                    <|> try (LiteralParam <$> nat)
                                    <|> try (PatternParam <$> patternParam)
                                    <|> WildcardParam <$ char '_')
                             comma)
  <* symbolic '=' <*> expr <* whiteSpace
  where convert name param ex = FuncDef name [(param, ex)]

constantDefinition :: Parser Definition
constantDefinition = ConstDef <$> var <* symbolic '=' <*> expr <* whiteSpace

lineComment :: Parser ()
lineComment = highlight H.Comment (string "--" *> skipMany (noneOf "\n")) *> whiteSpace

inlineComment :: Parser ()
inlineComment = highlight H.Comment (string "{-" *> manyTill anyChar (try (string "-}"))) *> whiteSpace

comment :: Parser ()
comment = some (lineComment <|> inlineComment) *> whiteSpace

program :: Parser Program
program = some (optional comment
                *> (try functionDefinition <|> constantDefinition)
                <* optional comment) <* eof

parse :: FilePath -> IO (Maybe Program)
parse = parseFromFile program
