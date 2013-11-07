
module LispError 
  (
  LispError(..), 
  ThrowsError, 
  trapError, 
  extractValue
  ) 
  where 

-- Error is Monad
import Control.Monad.Error
import Text.Parsec.Error
import Sexpr (LispVal)

-- Error constructors
data LispError =  NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | BadSpecialForm String LispVal
                | Parser ParseError
                | NotFunction String String
                | UnboundVariable String String
                | Default String

instance Show LispError where show = showError

showError :: LispError -> String

showError (NumArgs expected found) = 
    "expected " ++ show expected ++ " args"
    ++ "\nfound " ++ (show $ length found)
    ++ "\nvalue is : " ++ show found
showError (TypeMismatch expected found) = 
    "invalid type:\nExpected " ++ expected 
    ++ "\nfound " ++ show found -- should show type here
showError (Default errorMsg) = 
    "Unknown error: " ++ errorMsg
showError (UnboundVariable msg varName) = 
    "Unbound variable: " ++ varName ++ "\n" ++ msg
showError (NotFunction msg funcName) = 
    msg ++ ": " ++ funcName
showError (Parser parseError) = 
    "parse error: " ++ show parseError
showError (BadSpecialForm msg form) = 
    msg ++ ": " ++ show form


-- make it work with ghc's built-in error handling functions.
-- Error is also called the Exception Monad
instance Error LispError where
    -- noMsg :: LispError
    noMsg = Default "An error has occurred"
    -- hs default impl is noMsg = strMsg ""
    -- strMsg :: String -> LispError
    strMsg = Default

type ThrowsError = Either LispError
-- ThrowsError :: a -> Either LispError a
-- use a to represent functions that may throw exceptions

-- throwError :: e -> m a
-- or in our context
-- throwError :: LispError -> ThrowsError LispVal
-- ???

{-
 - Haskell's lazy nature makes exception handling fairly easy
 - Either is monadic so the message can be passed through 
 - without evaluate what comes after the exception
 -
 - exceptions in other langs are handled in a similar way
 - but they usually need special control flow to support this ?
 -}

-- do { action1; action2; action3 } `catchError` handler
-- catchError :: m a -> (e -> m a) -> m a
-- (e -> m a) is the error handling function

trapError action = catchError action (return . show)
-- we simply print the error message 

-- result of trapError is always, em, a Right Value
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- we do expect a runtime if not matched
