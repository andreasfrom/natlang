module Main where

-- TODO: Convert errors to Either and check a whole lot more conditions

import           AST
import           Control.Monad.Reader
import           Data.Maybe           (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid
import           Parser               (parse)
import           System.Environment   (getArgs)

eval :: Expr -> Reader Environment Int
eval (Number x) = return x
eval (Inc x) = liftM (1+) (eval x)
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

-- For the "f(a,a) implies a=a"-feature, there's a little bit too much implicit knowledge about the env being empty and only containing Numbers, but for now it works
matchesInst :: Instance -> [Int] -> Environment -> Maybe (Expr, Environment)
matchesInst (_,f) [] env = Just (f, env)
matchesInst (LiteralParam p : pars, f) (a:as) env
  | p == a = matchesInst (pars, f) as env
  | otherwise = Nothing
matchesInst (FreeParam p : pars, f) (a:as) env =
  case lookupEnv p env of
    Just (ConstDef _ (Number e)) -> if e == a then continue else Nothing
    Nothing -> continue
  where continue = matchesInst (pars,f) as (insertEnv (ConstDef p (Number a)) env)
matchesInst (PatternParam (Binding p) : pars, f) (a:as) env =
  case lookupEnv p env of
    Just (ConstDef _ (Number e)) -> if e == a then continue else Nothing
    Nothing -> continue
  where continue = matchesInst (pars,f) as (insertEnv (ConstDef p (Number a)) env)
matchesInst (PatternParam (Succ _) : _, _) (0:_) _ = Nothing
matchesInst (PatternParam (Succ s) : pars, f) (a:as) env =
  matchesInst (PatternParam s : pars, f) (a-1:as) env
matchesInst (WildcardParam : pars, f) (_:as) env = matchesInst (pars, f) as env

run :: Program -> Int
run defs = runReader prog mempty
  where prog = do
          let env = foldl (flip insertEnv) mempty defs
          case lookupEnv "main" env of
            Just (FuncDef _ [(_,m)]) -> local (env <>) (eval m)
            Just (ConstDef _ m) -> local (env <>) (eval m)
            _ -> error "No unique main function found."

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error "Supply the name of the file to run as the sole argument")
  ast <- parse (head args)
  case ast of
    Just prog -> print $ run prog
    Nothing -> return ()
