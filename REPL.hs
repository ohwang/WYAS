module REPL where

import Eval
import System.IO
import System.Console.Readline

import Data.Maybe (fromMaybe)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

lispPrompt = ">> "

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


-- an attempt to use monad style 
-- without knowing monad transformer

replReadline :: MaybeT IO String
replReadline = do
  line <- MaybeT $ readline lispPrompt
  lift $ addHistory line
  lift $ evalAndPrint line
  replReadline

runRepl = runMaybeT replReadline

-- old REPL without readline
mainLoop :: IO ()
mainLoop = do
  line <- getLine
  evalAndPrint line
  mainLoop

