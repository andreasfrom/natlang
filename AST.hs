module AST where

import qualified Data.Map       as Map
import           Data.Semigroup

type Name = String
type Form = String

data Pattern = Binding Name | Succ Pattern
             deriving (Show, Eq)

data Param = FreeParam Name | LiteralParam Int | PatternParam Pattern | WildcardParam
           deriving (Show, Eq)

data Expr = Number Int
          | Constant Name
          | Call Name [Expr]
          | Inc Expr
          deriving (Show, Eq)

type Instance = ([Param], Expr, Form)

data Definition = ConstDef Name Expr Form | FuncDef Name [Instance]
            deriving Show

instance Semigroup Definition where
  (FuncDef x xinsts) <> (FuncDef _ yinsts) = FuncDef x (yinsts ++ xinsts)
  x <> _ = x

newtype Environment = Environment (Map.Map Name Definition)
                 deriving Show

instance Monoid Environment where
  mempty = Environment Map.empty
  (Environment a) `mappend` (Environment b) = Environment (Map.unionWith (<>) a b)

instance Semigroup Environment where
  (<>) = mappend

showIntAsNat :: Int -> String
showIntAsNat 0 = "0"
showIntAsNat n = 'S' : showIntAsNat (pred n)

singletonEnv :: Name -> Definition -> Environment
singletonEnv n d = Environment $ Map.singleton n d

lookupEnv :: Name -> Environment -> Maybe Definition
lookupEnv n (Environment env) = Map.lookup n env

insertEnv :: Definition -> Environment -> Environment
insertEnv def@(ConstDef n _ _) env = singletonEnv n def <> env
insertEnv def@(FuncDef n _) env = singletonEnv n def <> env

defForm :: Definition -> [Form]
defForm (ConstDef _ _ f) = [f]
defForm (FuncDef _ is) = go is
  where go [] = []
        go ((_, _, f) : is') = f : go is'

type Program = [Definition]
