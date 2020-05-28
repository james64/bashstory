module Main where

import System.IO
import Lib
import GHC.IO.Encoding

loop :: IO ()
loop = do
    iseof <- hIsEOF stdin
    if iseof
        then return ()
        else do
            line <- getLine
            putStrLn line
            loop

main :: IO ()
main = do
    enc <- mkTextEncoding "UTF-8//IGNORE"
    hSetEncoding stdin enc
    loop
