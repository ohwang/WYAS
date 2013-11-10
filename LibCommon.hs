{-#LANGUAGE FlexibleInstances#-}
-- or equivalently can add -Xsomething as compiler args
--
-- Common pieces for library impl

module LibCommon where

import Sexpr (LispVal(..))
import LispError (ThrowsError, LispError(..))
import Control.Monad.Error (throwError)
import Control.Monad (liftM2)

class IsLisp a where
  fromLisp :: LispVal -> Maybe a
  fromLisp _ = Nothing
  toLisp :: a -> LispVal

instance IsLisp LispVal where
  fromLisp = Just
  toLisp = id

instance IsLisp Bool where
  fromLisp (Bool b) = Just b
  toLisp = Bool

-- requires FlexibleInstances
instance IsLisp [Char] where
  fromLisp (String s) = Just s
  fromLisp (Atom s) = Just s
  fromLisp _ = Nothing
  toLisp _ = undefined

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number val) = return val
unpackNum mismatched = throwError $ TypeMismatch "number" mismatched

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool val) = return val
unpackBool mismatched = throwError $ TypeMismatch "bool" mismatched

unpackStr :: LispVal -> ThrowsError String
unpackStr (String val) = return val
unpackStr mismatched = throwError $ TypeMismatch "string" mismatched

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinOp op [] = throwError $ NumArgs 2 []
numBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- lift funs to act on monads VS squeeze them into monadic env
numBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

{-
numBinOp op = foldM op' 
    where wrapNum x = Number x
          op' x y = liftM wrapNum (liftM2 op (unpackNum x) (unpackNum y))
-}

boolBinOp :: (LispVal -> ThrowsError a) 
              -> (a -> a -> Bool) 
              -> [LispVal] 
              -> ThrowsError LispVal

boolBinOp unpack binOp params = case params of 
      [x, y] -> liftM2 binOp (unpack x) (unpack y) >>= return . Bool
      _ -> throwError $ NumArgs 2 params

numBoolBinOp = boolBinOp unpackNum
boolBoolBinOp = boolBinOp unpackBool
strBoolBinOp = boolBinOp unpackStr

mkLispProc1 :: (IsLisp a, IsLisp b) => String ->
               (a -> b) -> [LispVal] -> ThrowsError LispVal
mkLispProc1 procName f args = case args of
  [x] -> case fromLisp x of
           -- can print out the type name here?
           Nothing -> throwError $ TypeMismatch ("argType for " ++ procName) x
           -- possible to fail because of (a -> b) ?
           Just y -> return $ toLisp (f y)
  badNumArgs -> throwError $ NumArgs 1 badNumArgs

mkLispProc2 :: (IsLisp a, IsLisp b, IsLisp c) =>
               String -> (a -> b -> c) -> [LispVal] -> ThrowsError LispVal
mkLispProc2 procName f args = case args of 
{-
  xs@[x1, x2] -> case mapM fromLisp xs of
                   Nothing     -> let misTyped = if fromLisp x1 == Nothing then x1 else x2 in
                                  throwError $ TypeMismatch ("argType for " ++ procName) misTyped
                   Just [x, y] -> return $ toLisp (f x y)
-- for this impl, hs will consider fromLisp to have the same type for x1 and x2
-}
  [x1, x2]   -> case fromLisp x1 of 
                  Nothing -> throwError $ TypeMismatch ("argType for " ++ procName) x1
                  Just y1 -> case fromLisp x2 of
                               Nothing -> throwError $ TypeMismatch ("argType for " ++ procName) x2
                               Just y2 -> return $ toLisp (f y1 y2)
  badNumArgs -> throwError $ NumArgs 2 badNumArgs

mkLispValp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
mkLispValp p [x] = return $ Bool (p x)
mkLispValp p badNumArgs = throwError $ NumArgs 1 badNumArgs

