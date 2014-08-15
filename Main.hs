module Main where

-- TODO: Convert errors to Either and check a whole lot more conditions
--       Do I need a monad transformer for this?
-- TODO: Look over parser
-- TODO: Move at least parser to own module

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

data Param = FreeParam Name | LiteralParam Nat | PatternParam Pattern
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

decNat :: Nat -> Nat
decNat Z = Z
decNat (S n) = n

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
      (params, f') = fromMaybe
                     (error $ "no suitable instance found: " ++ show insts)
                     (findInst insts args')
      env' = zipParams params args'
  return $ runReader (eval f') (env' <> env)

zipParams :: [Param] -> [Nat] -> Environment
zipParams [] _ = mempty
zipParams _ [] = mempty
zipParams (LiteralParam _:pars) (_:nats) = zipParams pars nats
zipParams (FreeParam p : pars) (n:nats) =
  ConstDef p (Number n) `insertEnv` zipParams pars nats
zipParams (PatternParam (Binding p) : pars) (n:nats) =
  ConstDef p (Number n) `insertEnv` zipParams pars nats
zipParams (PatternParam (Succ p) : pars) (n:nats) =
  zipParams (PatternParam p : pars) (decNat n:nats)

findInst :: [Instance] -> [Nat] -> Maybe Instance
findInst insts arguments =
  listToMaybe $ mapMaybe (`matchesInst` arguments) insts

matchesInst :: Instance -> [Nat] -> Maybe Instance
matchesInst (params, ex) args =
  case go (params, ex) args of
    Just f -> Just (params, f)
    Nothing -> Nothing
  where go :: Instance -> [Nat] -> Maybe Expr
        go (_,f') [] = Just f'
        go (LiteralParam p : pars, f') (a:as)
          | p == a = go (pars, f') as
          | otherwise = Nothing
        go (FreeParam _ : pars, f') (_:as) = go (pars,f') as
        go (PatternParam (Binding _) : pars, f') (_:as) = go (pars,f') as
        go (PatternParam (Succ s) : pars, f') (a:as)
          | a /= Z = go (PatternParam s : pars, f') (decNat a:as)
          | otherwise = Nothing
        go _ _ = Nothing

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
                                       <|> PatternParam <$> pattern) comma)
                     <* symbolic '=' <* whiteSpace
                     <*> expr <* whiteSpace
  where singleton name param ex = FuncDef name [(param, ex)]

constantDefinition :: Parser Definition
constantDefinition = ConstDef <$> var <* whiteSpace <* symbolic '=' <* whiteSpace <*> expr <* whiteSpace

program :: Parser Program
program = some (try functionDefinition <|> constantDefinition)

-- Runnning

parse :: IO Program
parse = do
   result <- parseFromFile program "toylang.tl"
   case result of
     Just res -> return res
     Nothing -> error "couldn't parse"

run :: Program -> Nat
run defs = runReader prog mempty
  where prog = do
          let env = foldl (flip insertEnv) mempty defs
          case lookupEnv "main" env of
            Just (FuncDef _ [(_,m)]) -> local (env `mappend`) (eval m)
            _ -> error "no (unique) main function"

main :: IO ()
main = do
   result <- parseFromFile program "natlang.nl"
   case result of
     Just prog -> let s = run prog in print (natToInt s, s)
     Nothing -> return ()
