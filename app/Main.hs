module Main where

import Lib
import Networks
import System.Environment

main = do
  args <- getArgs
  if head args == "client"
     then do
         clientPlayer
  else do
         serverPlayer
