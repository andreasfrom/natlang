module Parser (parse) where

import           AST
import           Control.Applicative
import           Text.Trifecta

expr :: Parser Expr
expr = try number <|> try func <|> constant <|> inc <|> parens expr

nat :: Parser Int
nat = convert <$> (many (char 'S') <* char 'Z')
  where convert [] = 0
        convert (_:xs) = 1 + convert xs

number :: Parser Expr
number = Number <$> nat

var :: Parser Name
var = some lower

constant :: Parser Expr
constant = Constant <$> var

func :: Parser Expr
func = Call <$> var <*> parens (sepBy expr comma)

inc :: Parser Expr
inc = Inc <$> (char 'S' *> some (char ' ') *> expr)

pattern :: Parser Pattern
pattern = convert <$> many (char 'S' <* whiteSpace) <*> var
  where convert [] n = Binding n
        convert (_:rest) n = Succ (convert rest n)

functionDefinition :: Parser Definition
functionDefinition = singleton <$> var
                     <*> parens (sepBy (FreeParam <$> var <|> try (LiteralParam <$> nat)
                                       <|> PatternParam <$> pattern
                                       <|> WildcardParam <$ char '_') comma)
                     <* symbolic '=' <* whiteSpace
                     <*> expr <* whiteSpace
  where singleton name param ex = FuncDef name [(param, ex)]

constantDefinition :: Parser Definition
constantDefinition = ConstDef <$> var <* whiteSpace <* symbolic '=' <* whiteSpace <*> expr <* whiteSpace

comment :: Parser ()
comment = string "--" *> many (noneOf "\n") *> whiteSpace

program :: Parser Program
program = some (optional comment *> (try functionDefinition <|> constantDefinition))

parse :: FilePath -> IO (Maybe Program)
parse = parseFromFile program
