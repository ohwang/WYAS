module Main where

import REPL (runRepl, simpleEvalLoop)
import System.IO

main = do 
  isTerm <- hIsTerminalDevice stdin
  if isTerm then runRepl >> return () else simpleEvalLoop
