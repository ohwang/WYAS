
module Eval 
  (
  evalString,
  evalAndPrint,
  ) where

import Sexpr
import LispError
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Control.Monad.Trans.Error hiding (throwError)

import qualified CoreLib as L

evalString :: String -> String
evalString s = extractValue $ trapError evaled
  -- evaled :: ThrowsError String
  where evaled = liftM show $ readExprM s >>= eval
  -- note how simple it is, 
  -- the monadic value need not be carried on in a single monadic env

evalAndPrint :: String -> IO ()
evalAndPrint = putStrLn . evalString

-- wrap the result in a simple Either
readExprM :: String -> ThrowsError LispVal
readExprM x = case parseLine x of
  (Left err) -> throwError $ Parser err 
  (Right val) -> return val

-------------------------
-- eval mechanism 
-- primitive functions are defined in CoreLib
--
-- quote and if are treatly specially
-- because only parts of the arguments are evaled
-------------------------

eval :: LispVal -> ThrowsError LispVal

-- primitives
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val

-- others
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, conseq, alt]) = do
    result <- eval cond
    case result of
      Bool False -> eval alt
      otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func

-- errors not caused by parsing
eval badForm = throwError $ BadSpecialForm "Bad Form" badForm 

--------------------------

apply :: String -> [LispVal] -> ThrowsError LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply func args = maybe (throwError $ NotFunction "unrecognized primitive func" func ) 
                        ($ args)
                        (lookup func L.primitives)
