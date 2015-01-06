module Main where

import Hash
import System.Environment

-- Main function which runs hash.
main :: IO ()
main = do
  args <- getArgs
  if null args then runInteractive
               else runScript $ head args
