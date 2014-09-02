module Parser (parse) where

import           AST
import           Control.Applicative
import qualified Data.ByteString.Char8       as BS
import qualified Text.Parser.Token.Highlight as H
import           Text.Trifecta

expr :: Parser Expr
expr = comment *> (callOrConstant <|> zero <|> incOrPos) <* comment

nat :: Parser Int
nat = highlight H.Number (length <$> many (char 'S') <* char '0'
                          <?> "literal natural number e.g. 0 or SS0") <* whiteSpace

zero :: Parser Expr
zero = highlight H.Number (char '0') *> pure (Number 0) <* whiteSpace

incOrPos :: Parser Expr
incOrPos = char 'S' *> (Inc <$> parens expr <|> Number <$> ((+1) <$> nat <?> "0, S0, SS0..."))

var :: Parser Name
var = highlight H.Identifier (try ((:) <$> lower <*> many alphaNum)) <* whiteSpace

callOrConstant :: Parser Expr
callOrConstant = do
  name <- var
  (Call name <$> parens (sepBy expr comma) <?> "function call e.g. func(param1, param2)")
    <|> ((return $ Constant name) <?> "parameter starting in lowercase e.g. n or myParam")

patternParam :: Parser Pattern
patternParam = try (convert <$> some (char 'S') <*> var)
               <?> "pattern for some successor of a free parameter e.g. SSn"
  where convert [] n = Binding n
        convert (_:rest) n = Succ (convert rest n)

definition :: Parser Definition
definition = slicedWith define $ do
  name <- var
  params <- optional $
    parens (sepBy (PatternParam <$> patternParam
                   <|> LiteralParam <$> nat
                   <|> FreeParam <$> var
                   <|> WildcardParam <$ (char '_' <?> "wildcard that matches everything but doesn't bind it: _"))
           comma)
  symbolic '='
  body <- expr
  return $ (name, params, body)
  where define (name, Nothing, body) form =
          ConstDef name body (BS.unpack (trimN form))
        define (name, Just params, body) form =
          FuncDef name [(params, body, BS.unpack (trimN form))]

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

trimN :: BS.ByteString -> BS.ByteString
trimN t = if BS.last t == '\n' then trimN (BS.init t) else t
