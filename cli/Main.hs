module Main where

import Text.JSON (fromFile)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then hPutStr stderr "json: expecting path to JSON file\n"
        else parse (head args)

parse :: String -> IO ()
parse path = do
    result <- fromFile path
    case result of
      Left err -> print err
      Right json -> print json
