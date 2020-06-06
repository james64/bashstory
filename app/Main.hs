module Main where

import System.IO ( stdin, hSetEncoding )
import System.Environment ( getArgs )
import Data.List ( foldl', isSuffixOf )
import GHC.IO.Encoding ( mkTextEncoding )

-- arg parsing
import Options.Applicative
import Data.Semigroup ((<>))

-- Argument parsing --
data Options = Options
  { sessionSeparator :: String
  , current :: Bool }

currentP :: Parser Bool
currentP = switch ( long "current" <> short 'c' <> help "Commands from current session only" )

sessionSepP :: Parser String
sessionSepP = strOption
  ( long "session-delim"
  <> help "Session separator string"
  <> showDefault
  <> value "### Session ###" )

options :: Parser Options
options = Options <$> sessionSepP <*> currentP

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> header "Extension to bash history builtin" )

run :: Options -> IO ()
run (Options _ False) = interact id
run (Options ss True)  = currSessIO ss

main :: IO ()
main = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding stdin enc
  run =<< execParser opts

currentSession :: String -> [String] -> [String]
currentSession sesDel = reverse . foldl' lastS []
    where
        lastS :: [String] -> String -> [String]
        lastS _ l | isDelim l = []
        lastS s l             = l : s

        isDelim = isSuffixOf sesDel

currSessIO :: String -> IO ()
currSessIO sesDel = do
    input <- getContents
    mapM_ putStrLn $ id $ currentSession sesDel (lines input)
