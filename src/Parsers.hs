module Parsers
    ( opts
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex.TDFA

import GlobalTypes

reHelper :: String -> String -> [String]
reHelper re s | (_,_,_,grps) <- (s =~ re) :: (String, String, String,[String]) = grps
              | otherwise                                                      = []

isRelTime = reHelper "^([0-9])([smhd])$"
isAbsTime = reHelper "^[0-9]{2}:[0-9]{2}:[0-9]{2}$" 

readTimeValue :: ReadM TimeFilter
readTimeValue = eitherReader $ parse
  where parse :: String -> Either String TimeFilter
        parse s | [num,u] <- isRelTime s = Right $ RelTimeFilter (read num) $ case u of
                    "s" -> Seconds
                    "m" -> Minutes
                    "h" -> Hours
                    "d" -> Days
                | [h,m,s] <- isAbsTime s = Right $ AbsTimeFilter (read h) (read m) (read s)
                | otherwise = Left $ "Unknown format: " ++ s

beforeP :: Parser TimeFilter
beforeP = option readTimeValue
   ( long "before"
   <> short 'b'
   <> help" Take only commands beffore given time. Format: <num>[smhd] or HH:MM:SS" )

afterP :: Parser TimeFilter
afterP = option readTimeValue
   ( long "after"
   <> short 'a'
   <> help "Take only commands after given time. Format: <num>[smhd] or HH:MM:SS" )

currentP :: Parser Bool
currentP = switch ( long "current" <> short 'c' <> help "Commands from current session only" )

sessionSepP :: Parser String
sessionSepP = strOption
  ( long "session-delim"
  <> help "Session separator string"
  <> showDefault
  <> value "### Session ###" )

options :: Parser Options
options = Options <$> sessionSepP <*> currentP <*> afterP <*> beforeP

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> header "Extension to bash history builtin" )

