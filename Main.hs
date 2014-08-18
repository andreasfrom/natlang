module Main where

-- TODO: Convert errors to Either and check a whole lot more conditions
-- TODO: Look over parser
-- TODO: Move at least parser to own module
-- TODO: Some form of boolean expressions?

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid          hiding ((<>))
import           Data.Semigroup
import           Text.Trifecta

type Name = String

data Pattern = Binding Name | Succ Pattern
             deriving (Show, Eq)

data Param = FreeParam Name | LiteralParam Nat | PatternParam Pattern | WildcardParam
           deriving (Show, Eq)

data Nat = Z | S Nat
         deriving (Show, Eq)

data Expr = Number Nat
          | Constant Name
          | Call Name [Expr]
          | Inc Expr
          deriving Show

type Instance = ([Param], Expr)

data Definition = ConstDef Name Expr | FuncDef Name [Instance]
            deriving Show

instance Semigroup Definition where
  (FuncDef x xinsts) <> (FuncDef _ yinsts) = FuncDef x (yinsts <> xinsts)
  x <> _ = x

newtype Environment = Environment (Map.Map Name Definition)
                 deriving Show

instance Monoid Environment where
  mempty = Environment Map.empty
  (Environment a) `mappend` (Environment b) = Environment (Map.unionWith (<>) a b)

instance Semigroup Environment where
  (<>) = mappend

singletonEnv :: Name -> Definition -> Environment
singletonEnv n d = Environment $ Map.singleton n d

lookupEnv :: Name -> Environment -> Maybe Definition
lookupEnv n (Environment env) = Map.lookup n env

insertEnv :: Definition -> Environment -> Environment
insertEnv def@(ConstDef n _) env = singletonEnv n def <> env
insertEnv def@(FuncDef n _) env = singletonEnv n def <> env

type Program = [Definition]

natToInt :: Nat -> Integer
natToInt = go 0
  where go acc Z = acc
        go acc (S n) = go (acc+1) n

eval :: Expr -> Reader Environment Nat
eval (Number x) = return x
eval (Inc x) = liftM S (eval x)
eval (Constant n) = do
  env <- ask
  case lookupEnv n env of
    Just (ConstDef _ n') -> eval n'
    _ -> error $ "No constant found called: " ++ n ++ " in environment: " ++ show env
eval (Call f args) = do
  env <- ask
  let args' = map ((`runReader` env) . eval) args
      insts = case fromMaybe (error $ "unknown function: " ++ show f) (lookupEnv f env) of
        (FuncDef _ is) -> is
        _ -> error "no function"
      (f', env') = fromMaybe (error $ "No suitable instance found: " ++ show insts
                              ++ "\nFor args: " ++ show args')
                   (listToMaybe $ mapMaybe (\i -> matchesInst i args' mempty) insts)
  return $ runReader (eval f') (env' <> env)

matchesInst :: Instance -> [Nat] -> Environment -> Maybe (Expr, Environment)
matchesInst (_,f) [] env = Just (f, env)
matchesInst (LiteralParam p : pars, f) (a:as) env
  | p == a = matchesInst (pars, f) as env
  | otherwise = Nothing
matchesInst (FreeParam p : pars, f) (a:as) env =
  matchesInst (pars,f) as (insertEnv (ConstDef p (Number a)) env)
matchesInst (PatternParam (Binding p) : pars, f) (a:as) env =
  matchesInst (pars,f) as (insertEnv (ConstDef p (Number a)) env)
matchesInst (PatternParam (Succ _) : _, _) (Z:_) _ = Nothing
matchesInst (PatternParam (Succ s) : pars, f) (S a:as) env =
  matchesInst (PatternParam s : pars, f) (a:as) env
matchesInst (WildcardParam : pars, f) (_:as) env = matchesInst (pars, f) as env

-- Parser

expr :: Parser Expr
expr = try number <|> try func <|> constant <|> inc <|> parens expr

nat :: Parser Nat
nat = convert <$> (many (char 'S') <* char 'Z')
  where convert [] = Z
        convert (_:xs) = S (convert xs)

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

-- Runnning

parse :: FilePath -> IO Program
parse file = do
   result <- parseFromFile program file
   case result of
     Just res -> return res
     Nothing -> error "couldn't parse"

run :: Program -> Nat
run defs = runReader prog mempty
  where prog = do
          let env = foldl (flip insertEnv) mempty defs
          case lookupEnv "main" env of
            Just (FuncDef _ [(_,m)]) -> local (env <>) (eval m)
            Just (ConstDef _ m) -> local (env <>) (eval m)
            _ -> error "no (unique) main function"

main :: IO ()
main = parse "natlang.nl" >>= \prog -> let s = run prog in print (natToInt s, s)
