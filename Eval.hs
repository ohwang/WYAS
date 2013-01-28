{-#LANGUAGE FlexibleInstances#-}

-- scheme 48

module Eval where
import Sexpr

main :: IO ()
main = getLine >>= putStrLn . show . eval. readExpr' >> main

-------------------------

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@_ = error $ "Not implemented " ++ show val

--------------------------

apply :: String -> [LispVal] -> LispVal
-- give unsucessful evaluation the value of False
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
            ("+", numericBinOp (+))
           ,("-", numericBinOp (-))
           ,("*", numericBinOp (*))
           ,("/", numericBinOp div)
           ,("mod", numericBinOp mod)
           ,("quotient", numericBinOp quot)
           ,("remainder", numericBinOp rem)
           ,("symbol?", mkLispValp isSymbol)
           ,("string?",mkLispValp isString)
           ,("number?",mkLispValp isNumber)
           ,("symbol->string", symbolToString)
            ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal

{- the last parenthese is not necessary, because of currying
 - and thus redundent parameters exist
 - -}

numericBinOp op = foldl1 op'
    where unwrapNum :: LispVal -> Integer
          wrapNum = Number
          op' x y = wrapNum (op (unwrapNum x) (unwrapNum y))
          unwrapNum x = case x of
            Number val -> val
            _ -> error "Can not compute on not number"

{-
          op' x y = wrapNum . op (unwrapNum x) . unwrapNum $ y
          op' x = wrapNum . op (unwrapNum x) . unwrapNum
          op' x = (wrapNum . op (unwrapNum x)) . (unwrapNum)
          op' x = (.) (wrapNum . op (unwrapNum x)) unwrapNum
          op' x = flip (.) unwrapNum (wrapNum . op (unwrapNum x))
          op' x = flip (.) unwrapNum . (wrapNum . op) . unwrapNum $ x
          op' = flip (.) unwrapNum . (wrapNum . op) . unwrapNum
-}

{-
numericBinOp op params = Number $ f o l d l 1 op $ map unpackNum params

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

instance IsLisp [Char] where
  fromLisp (String s) = Just s
  fromLisp (Atom s) = Just s
  fromLisp _ = Nothing
  toLisp _ = undefined

mkLispProc1 :: (IsLisp a, IsLisp b) => String ->
               (a -> b) -> [LispVal] -> LispVal
mkLispProc1 procName f args = case args of
  [a] -> case fromLisp a of
    Nothing -> error $ "Type error at: " ++ procName
    Just a -> toLisp (f a)
  _ -> error $ "Wrong arg count at: " ++ procName

mkLispValp :: (LispVal -> Bool) -> [LispVal] -> LispVal
mkLispValp p [x] = Bool $ p x
mkLispValp p _ = error "Wrong number of arguments"

isNumber x = case x of {Number _ -> True; _ -> False}
isSymbol x = case x of {Atom _ -> True; _ -> False}
isString x = case x of {String _ -> True; _ -> False}

symbolToString = mkLispProc1 "stringToSymbol" String

