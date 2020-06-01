module Main where

import System.IO ( stdin, hSetEncoding )
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Data.List ( foldl', isSuffixOf )
import GHC.IO.Encoding ( mkTextEncoding )

sessionDelimiter :: String
sessionDelimiter = "### Session ###"

currentSession :: [String] -> [String]
currentSession = reverse . foldl' lastS []
    where
        lastS :: [String] -> String -> [String]
        lastS _ l | isDelim l = []
        lastS s l             = l : s

        isDelim = isSuffixOf sessionDelimiter

currSessIO :: IO ()
currSessIO = do
    input <- getContents
    mapM_ putStrLn $ id $ currentSession (lines input)

main :: IO ()
main = do
    enc <- mkTextEncoding "UTF-8//IGNORE"
    hSetEncoding stdin enc
    args <- getArgs
    case args of
        []       -> interact id
        ["curr"] -> currSessIO
        _        -> exitWith (ExitFailure 1)

