module Parser (parse) where

import           AST
import           Control.Applicative
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta

expr :: Parser Expr
expr = comment *> (call <|> constant <|> zero <|> incOrPos) <* comment

nat :: Parser Int
nat = highlight H.Number (length <$> many (char 'S') <* char '0'
                          <?> "literal natural number e.g. 0 or SS0") <* whiteSpace

zero :: Parser Expr
zero = highlight H.Number (char '0') *> pure (Number 0) <* whiteSpace

incOrPos :: Parser Expr
incOrPos = do
  char 'S'
  arg <- optional (parens expr)
  case arg of
   Just arg' -> return $ Inc arg'
   Nothing -> Number <$> (1+) <$> nat

var :: Parser Name
var = highlight H.Identifier (try ((:) <$> lower <*> many alphaNum)) <* whiteSpace

constant :: Parser Expr
constant = Constant <$> var
           <?> "parameter starting in lowercase e.g. n or myParam"

call :: Parser Expr
call = try (Call <$> var <*> parens (sepBy expr comma))
       <?> "function call e.g. func(param1, param2)"

patternParam :: Parser Pattern
patternParam = try (convert <$> some (char 'S') <*> var)
               <?> "pattern for some successor of a free parameter e.g. SSn"
  where convert [] n = Binding n
        convert (_:rest) n = Succ (convert rest n)

definition :: Parser Definition
definition = do
  name <- var
  params <- optional $
    parens (sepBy (PatternParam <$> patternParam
                   <|> LiteralParam <$> nat
                   <|> FreeParam <$> var
                   <|> WildcardParam <$ (char '_' <?> "wildcard that matches everything but doesn't bind it: _"))
           comma)
  symbolic '='
  body <- expr
  return $ case params of
            Just ps -> FuncDef name [(ps, body)]
            Nothing -> ConstDef name body


lineComment :: Parser ()
lineComment = highlight H.Comment ((string "--" *> skipMany (noneOf "\n"))
              <?> "comment spanning rest of line: -- comment") *> whiteSpace

inlineComment :: Parser ()
inlineComment = highlight H.Comment
                ((string "{-" *> manyTill anyChar (try (string "-}")))
                 <?> "inline comment: {- comment -}") *> whiteSpace

comment :: Parser ()
comment = many (lineComment <|> inlineComment) *> whiteSpace

program :: Parser Program
program = some (comment *> definition <* comment) <* eof

parse :: FilePath -> IO (Maybe Program)
parse = parseFromFile program
