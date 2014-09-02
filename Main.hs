module Main where

import           AST
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromJust, listToMaybe, mapMaybe)
import           Data.Monoid
import           Parser                     (parse)
import           System.Environment         (getArgs)

type ExceptReader = ExceptT String (Reader Environment) Int

eval :: Expr -> ExceptReader
eval (Number x) = return x
eval (Inc x) = liftM (1+) (eval x)
eval (Constant n) = ask >>= \env ->
  case lookupEnv n env of
    Just (ConstDef _ n' form') -> withExceptT (++ "\n\nin constant " ++ form') (eval n')
    _ -> throwE $ "Unknown parameter " ++ n ++ "\n"
eval (Call f args) = do
  env <- ask
  args' <- mapExceptT (return . (`runReader` env)) (sequence (map eval args))
  insts <- case (lookupEnv f env) of
    Just (FuncDef _ is) -> return is
    Nothing -> throwE $ "unknown function " ++ show f
  case listToMaybe $ mapMaybe (\i -> matchesInst i args' mempty) insts of
     Just (f', env') -> mapExceptT (return . (`runReader` (env' <> env))) (eval f')
     Nothing -> throwE $ "No suitable instance of " ++ show f
                ++ " among these:\n\t"
                ++ intercalate "\n\t" (map frm insts) ++ "\n\n"
                ++ "found for arg(s):\n\t"
                ++ intercalate "\n\t" (map (\a -> showIntAsNat a ++ "  :  " ++ show a) args')
       where frm (_,_,x) = x

findEnvs :: [Name] -> Environment -> [Form]
findEnvs = go []
  where go forms [] _ = forms
        go forms (n:ns) env = go (defForm (fromJust (lookupEnv n env)) ++ forms) ns env

-- For the "f(a,a) implies a=a"-feature, there's a little bit too much implicit knowledge about the env being empty and only containing Numbers, but for now it works
matchesInst :: Instance -> [Int] -> Environment -> Maybe (Expr, Environment)
matchesInst ([],f,_) [] env = Just (f, env)
matchesInst (LiteralParam p : pars, f, form) (a:as) env
  | p == a = matchesInst (pars, f, form) as env
  | otherwise = Nothing
matchesInst (FreeParam p : pars, f, form) (a:as) env =
  case lookupEnv p env of
    Just (ConstDef _ (Number e) _) -> if e == a then continue else Nothing
    Nothing -> continue
  where continue = matchesInst (pars,f,form) as (insertEnv (ConstDef p (Number a) form) env)
matchesInst (PatternParam (Binding p) : pars, f, form) (a:as) env =
  case lookupEnv p env of
    Just (ConstDef _ (Number e) _) -> if e == a then continue else Nothing
    Nothing -> continue
  where continue = matchesInst (pars,f,form) as (insertEnv (ConstDef p (Number a) form) env)
matchesInst (PatternParam (Succ _) : _, _, _) (0:_) _ = Nothing
matchesInst (PatternParam (Succ s) : pars, f, form) (a:as) env =
  matchesInst (PatternParam s : pars, f, form) (a-1:as) env
matchesInst (WildcardParam : pars, f, form) (_:as) env = matchesInst (pars, f, form) as env
matchesInst _ _ _ = Nothing

run :: Program -> Either String Int
run defs = case runReader (runExceptT prog) mempty of
  Right i -> Right i
  Left err -> Left err
  where prog = do
          let env = foldl (flip insertEnv) mempty defs
          case lookupEnv "main" env of
            Just (FuncDef _ [(_,m,_)]) -> local (env <>) (eval m)
            Just (ConstDef _ m _) -> local (env <>) (eval m)
            _ -> error "No unique main function found."

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error "Supply the name of the file to run as the sole argument")
  ast <- parse (head args)
  case ast of
    Just prog -> either putStrLn print (run prog)
    Nothing -> return ()
