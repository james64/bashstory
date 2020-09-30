module Main where

import System.IO ( stdin, hSetEncoding )
import System.Environment ( getArgs )
import GHC.IO.Encoding ( mkTextEncoding )

import Options.Applicative ( execParser )

import GlobalTypes
import Parsers
import Routines.CurrSession ( currSessIO )
import Routines.FilterByTime ( filterByTimeIO )

run :: Options -> IO ()
run (Options _  False []) = interact id
run (Options ss True [])  = currSessIO ss
run (Options ss curr fs)  = filterByTimeIO ss curr fs

main :: IO ()
main = do
  enc <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding stdin enc
  run =<< execParser opts

