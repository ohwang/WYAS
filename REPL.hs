module REPL where

import Eval
import System.IO
import System.IO.Error (catchIOError, isEOFError)
import System.Console.Readline

import Data.Maybe (fromMaybe)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

lispPrompt = "-> "

flushStr :: String -> IO ()
-- no newline
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
-- getLine is too simple and naive ?
-- what if EOF is encountered, should return ``Maybe String`` instead?

-- xxx_ means a monadic computation that 
-- never terminate / return nothing
--
-- without monad transformer
until_' pred prompt action = do
  -- line :: Maybe String
  maybeLine <- prompt
  case maybeLine of
    Nothing -> return ()
    Just line -> if pred line 
                  then return ()
                  else do addHistory line
                          action line
                          until_' pred prompt action

runRepl' :: IO ()
runRepl' = until_' quitPred (readline lispPrompt) evalAndPrint
  where quitPred = (== "quit")

replReadline :: MaybeT IO String
replReadline = do
  line <- MaybeT $ readline lispPrompt
  lift $ addHistory line
  lift $ evalAndPrint line
  replReadline

runRepl = runMaybeT replReadline

-- old REPL without readline
-- functions cannot begin with an uppercase letter
simpleEvalLoop :: IO ()
simpleEvalLoop = do
  line <- getLine `catchIOError` eofErrorHandler
  if line /= "\0"
    then evalAndPrint line >> simpleEvalLoop 
    else return ()
  where 
    eofErrorHandler e = if isEOFError e then return "\0" else ioError e

