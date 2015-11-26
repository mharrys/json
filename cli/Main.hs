module Main where

import Text.JSON
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
    args <- getArgs
    case (null args) of
      True -> hPutStr stderr "json: expecting path to JSON file\n"
      False -> parse (head args)

parse :: String -> IO ()
parse path = do
    result <- parseFile path
    case result of
      Left err -> print err
      Right json -> print json
