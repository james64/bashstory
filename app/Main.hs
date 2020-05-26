module Main where

import System.IO
import Lib

main :: IO ()
main = do
    iseof <- hIsEOF stdin
    if iseof
        then return ()
        else do
            line <- getLine
            putStrLn line
            main
