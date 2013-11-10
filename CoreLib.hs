-- Core Library Impl

module CoreLib where

import Sexpr (LispVal(..))
import LispError (ThrowsError, LispError(..))
import Control.Monad.Error (throwError)
import LibCommon

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
           ,("and", boolBoolBinOp (&&))
           ,("or", boolBoolBinOp (||))
           ,("not", boolNot)
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

{-
    op' x y = wrapNum . op (unpackNum x) . unpackNum $ y
    op' x = wrapNum . op (unpackNum x) . unpackNum
    op' x = (wrapNum . op (unpackNum x)) . (unpackNum)
    op' x = (.) (wrapNum . op (unpackNum x)) unpackNum
    op' x = flip (.) unpackNum (wrapNum . op (unpackNum x))
    op' x = flip (.) unpackNum . (wrapNum . op) . unpackNum $ x
    op' = flip (.) unpackNum . (wrapNum . op) . unpackNum
-}

isNumber x = case x of {Number _ -> True; _ -> False}
isSymbol x = case x of {Atom _ -> True; _ -> False}
isString x = case x of {String _ -> True; _ -> False}

symbolToString = mkLispProc1 "symbolToString" String
boolNot = mkLispProc1 "not" not

----- car cdr cons -----

-- to pattern match top level list elem you don't need parens
-- note that arguments should still be evaluated
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
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

