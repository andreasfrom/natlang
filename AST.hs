module AST where

import qualified Data.Map       as Map
import           Data.Semigroup

type Name = String

data Pattern = Binding Name | Succ Pattern
             deriving (Show, Eq)

data Param = FreeParam Name | LiteralParam Int | PatternParam Pattern | WildcardParam
           deriving (Show, Eq)

data Expr = Number Int
          | Constant Name
          | Call Name [Expr]
          | Inc Expr
          deriving (Show, Eq)

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
