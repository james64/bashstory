module Main where

import System.IO ( stdin, hSetEncoding )
import System.Environment ( getArgs )
import Data.List ( foldl', isSuffixOf )
import GHC.IO.Encoding ( mkTextEncoding )

-- arg parsing
import Options.Applicative
import Data.Semigroup ((<>))

-- Argument parsing --
data Options = Options { current :: Bool }

currentP :: Parser Bool
currentP = switch ( long "current" <> short 'c' <> help "Commands from current session only" )

options :: Parser Options
options = Options <$> currentP

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> progDesc "Print history list"
  <> header "this is helper header" )

run :: Options -> IO ()
run (Options False) = interact id
run (Options True)  = currSessIO

main :: IO ()
main = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding stdin enc
  run =<< execParser opts

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
