-- scheme 48
--
module Sexpr (parseExpr, readExpr', LispVal(..)) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.List
import Control.Monad

{-Needed?-}
import Text.Parsec.Error

main :: IO ()
main = do 
    str <- getLine
    putStrLn $ readExpr str

readExpr' :: String -> LispVal
readExpr' input = case parse (spaces >> parseExpr) "scheme" input of
        Left err -> error $ "Parse Error " ++ show err
        Right val -> val

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "scheme" input of
        Left err -> "Parse Error : " ++ show err
        Right val -> show val

----------------------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show val = case val of
        Atom str -> str
        List xs -> "(" ++ unwordsList xs ++ ")"
        DottedList xs x ->"(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
        String str -> show str
        Bool bool -> if bool then "#t" else "#f"
        Number num -> show num

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

---------------------------------------------

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/#:<=>?@^_~"

quoteTypes = [("'", "quote"), ("`", "backquote")]

mkQuotedParser :: [(String, String)] -> Parser LispVal
mkQuotedParser xs = foldr1 (<|>) (map (\(x, y) -> parseQuoted x y) xs)

parseExpr :: Parser LispVal
parseExpr = try parseAtom
        <|> parseString
        <|> parseNumber
        <|> mkQuotedParser quoteTypes
        <|> do char '('
               x <- parseList'
               char ')'
               return x

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = liftM Number pInteger

parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol )
            let atom = first : rest
            return $ case atom of 
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _  -> Atom atom

wrapped :: Char -> Char -> Parser a -> Parser a
wrapped pre post m = do char pre
                        a <- m
                        char post
                        return a

parseAnotherList = wrapped '[' ']' $ do
    lst <- sepBy parseExpr spaces
    return $ List lst

lexer = T.makeTokenParser emptyDef

pStr :: Parser String
pStr = T.stringLiteral lexer

pInteger :: Parser Integer
pInteger = T.integer lexer

{-
 - pStr :: Parser String | m a, where m = Parser, a = String
 - pLispString :: Parser LispVal | m b, where m = Parser, b = LispVal
 - String :: String -> LispVal | a -> b, where a = ..., b = ...
 - return :: b -> m b
 - (>>=) :: m a -> (a -> m b) -> m b
 - (.) :: (b -> c) -> (a -> b) -> a -> c
 -
pLispString :: Parser LispVal
pLispString = do str <- pStr
                 return $ String str
 -}

parseString :: Parser LispVal
parseString = liftM String pStr

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseList' :: Parser LispVal
parseList' = do
    head <- endBy parseExpr spaces
    tail <- (liftM Just) (char '.' >> spaces >> parseExpr) <|> return Nothing
    return $ case tail of
        Nothing -> List head
        Just xs  -> DottedList head xs

-- modify to support quasiquoataion
parseQuoted :: String -> String -> Parser LispVal
parseQuoted leadingSymbol quoteType = do
    string leadingSymbol
    x <- parseExpr
    return $ List [Atom quoteType, x]

