{-#LANGUAGE FlexibleInstances#-}
-- or equivalently can add -Xsomething as compiler args

module CoreLib where

import Sexpr (LispVal(..))
import LispError (ThrowsError, LispError(..))
import Control.Monad.Error (throwError)
import Control.Monad (liftM2)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
            ("+", numBinOp (+))
           ,("-", numBinOp (-))
           ,("*", numBinOp (*))
           ,("/", numBinOp div)
           ,("mod", numBinOp mod)
           ,("quotient", numBinOp quot)
           ,("remainder", numBinOp rem)
           ------
           ,("symbol?", mkLispValp isSymbol)
           ,("string?",mkLispValp isString)
           ,("number?",mkLispValp isNumber)
           ------
           ,("symbol->string", symbolToString)
           ------
           ,("=", numBoolBinOp (==))
           ,("<=", numBoolBinOp (<=))
           ,(">=", numBoolBinOp (>=))
           ,("/=", numBoolBinOp (/=))
           ,("<", numBoolBinOp (>))
           ,(">", numBoolBinOp (<))
           ------
           ,("&&", boolBoolBinOp (&&))
           ,("||", boolBoolBinOp (||))
           ------
           ,("string=?", strBoolBinOp (==))
           ,("string<?", strBoolBinOp (<))
           ,("string>?", strBoolBinOp (>))
           ,("string<=?", strBoolBinOp (<=))
           ,("string>=?", strBoolBinOp (>=))
           ------
           ,("car", car)
           ,("cdr", cdr)
           ,("cons", cons)
            ]

numBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal

numBinOp op [] = throwError $ NumArgs 2 []

numBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- not necessary to manually lift normal functions to do monadic computation
-- it's wiser to wrap them in a monadic environment
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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number val) = return val
unpackNum mismatched = throwError $ TypeMismatch "number" mismatched

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool val) = return val
unpackBool mismatched = throwError $ TypeMismatch "bool" mismatched

-- why does he sliently convert not-string to string
-- only because they can be interpreted as strings
unpackStr :: LispVal -> ThrowsError String
unpackStr (String val) = return val
unpackStr mismatched = throwError $ TypeMismatch "string" mismatched

{-
    op' x y = wrapNum . op (unpackNum x) . unpackNum $ y
    op' x = wrapNum . op (unpackNum x) . unpackNum
    op' x = (wrapNum . op (unpackNum x)) . (unpackNum)
    op' x = (.) (wrapNum . op (unpackNum x)) unpackNum
    op' x = flip (.) unpackNum (wrapNum . op (unpackNum x))
    op' x = flip (.) unpackNum . (wrapNum . op) . unpackNum $ x
    op' = flip (.) unpackNum . (wrapNum . op) . unpackNum
-}

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

mkLispProc1 :: (IsLisp a, IsLisp b) => String ->
               (a -> b) -> [LispVal] -> ThrowsError LispVal
mkLispProc1 procName f args = case args of
  [a] -> case fromLisp a of
    -- can print out the type name here?
    Nothing -> throwError $ TypeMismatch ("argType for " ++ procName) a
    -- possible to fail because of (a -> b) ?
    Just x -> return $ toLisp (f x)
  badNumArgs -> throwError $ NumArgs 1 badNumArgs

mkLispValp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
mkLispValp p [x] = return $ Bool (p x)
mkLispValp p badNumArgs = throwError $ NumArgs 1 badNumArgs

isNumber x = case x of {Number _ -> True; _ -> False}
isSymbol x = case x of {Atom _ -> True; _ -> False}
isString x = case x of {String _ -> True; _ -> False}

symbolToString = mkLispProc1 "symbolToString" String

--- car cdr cons 

-- to pattern match top level list elem you don't need parens
-- note that arguments should still be evaluated
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
-- not sure about R5RS standard
cdr [DottedList [_] y] = return y
cdr [DottedList (x:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- cons takes two arguments
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
-- this cathes all cases for 2-list
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x,y] = return $ Bool $ x == y
eqv badArgList = throwError $ NumArgs 2 badArgList

-- extensions are very common for Haskell
-- Usually necessary to create reasonably large program

