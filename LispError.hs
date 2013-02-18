
module LispError where 

import Control.Monad.Error
import Text.Parsec.Error

data LispError = NumArgs Integer [LispVal]
                 TypeMismatch String LispVal
                 Default String

instance Show LispError where show = showError

showError :: LispError -> String

showError NumArgs expected found = "Expected " ++ show expected ++ " args"
                    ++ "\nFound " ++ (show $ length found)
                    ++ "\nValue is : " ++ found

showError TypeMismatch expected found = "Invalid Type:\nExpected " ++ expected 
                    ++ "\nFound " ++ found


