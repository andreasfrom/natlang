module Main where

import           AST
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.List                  (intercalate)
import           Data.Maybe                 (listToMaybe, mapMaybe)
import           Data.Monoid
import           Parser                     (parse)
import           System.Environment         (getArgs)

type ExceptReader = ExceptT String (Reader Environment) Int

eval :: Expr -> ExceptReader
eval (Number x) = return x
eval (Inc x) = liftM (1+) (eval x)
eval (Constant n) = ask >>= \env ->
  case lookupEnv n env of
    Just (ConstDef _ n') -> withExceptT (++ "\n\tin constant " ++ show n) (eval n')
    _ -> throwE $ "Unknown parameter " ++ show n
eval (Call f args) = do
  env <- ask
  args' <- mapExceptT (return . (`runReader` env)) (sequence (map (context . eval) args))
  insts <- case (lookupEnv f env) of
    Just (FuncDef _ is) -> return is
    Nothing -> throwE $ "unknown function " ++ show f
  case listToMaybe $ mapMaybe (\i -> matchesInst i args' mempty) insts of
     Just (f', env') -> mapExceptT (return . (`runReader` (env' <> env))) (eval f')
     Nothing -> throwE $ "No suitable instance of " ++ show f
                ++ " among these:\n\t"
                ++ intercalate "\n\t" (map (show . fst) insts)
                ++ "\n\tfound for arg(s):\n\t\t"
                ++ intercalate "\n\t\t" (zipWith zipper args' args)
       where zipper a a' = show a ++ " from \"" ++ show a' ++ "\""
    where context = withExceptT (++ "\n\tin call to " ++ show f ++ " with " ++ show args)

-- For the "f(a,a) implies a=a"-feature, there's a little bit too much implicit knowledge about the env being empty and only containing Numbers, but for now it works
matchesInst :: Instance -> [Int] -> Environment -> Maybe (Expr, Environment)
matchesInst ([],f) [] env = Just (f, env)
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
matchesInst _ _ _ = Nothing

run :: Program -> Either String Int
run defs = case runReader (runExceptT prog) mempty of
  Right i -> Right i
  Left err -> Left err
  where prog = do
          let env = foldl (flip insertEnv) mempty defs
          case lookupEnv "main" env of
            Just (FuncDef _ [(_,m)]) -> local (env <>) (context (eval m))
            Just (ConstDef _ m) -> local (env <>) (context (eval m))
            _ -> error "No unique main function found."
        context = withExceptT (++ "\n\tin call to main")

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error "Supply the name of the file to run as the sole argument")
  ast <- parse (head args)
  case ast of
    Just prog -> either putStrLn print (run prog)
    Nothing -> return ()
