module Parsers
    ( opts
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex.TDFA

import GlobalTypes

let relTimeRe = "^([0-9])([smhd])$" :: String
let absTimeRe = "^[0-9]{2}:[0-9]{2}:[0-9]{2}$" :: String

readTimeValue :: ReadM RelTimeFilter
readTimeValue = eitherReader $ parse
  where parse :: String -> Either String RelTimeFilter
        parse s | (_,_,_,[cnt,u]) <- (s =~ relTimeRe) :: (String,String,String,[String]) = RelTimeFilter (read cnt) $ case u of
                    's' -> Seconds
                    'm' -> Minutes
                    'h' -> Hours
                    'd' -> Days
                | (_,_,_,[h,m,s]) <- (s =~ absTimeRe) :: (String,String,String,[String]) = Rel

beforeP :: Parser RelTimeFilter
beforeP :: option readTimeValue
  ( long "before"
  <> short 'b'
  <> help" Take only commands beffore given time. Format: <num>[smhd] or HH:MM:SS"

afterP :: Parser RelTimeFilter
afterP = option readTimeValue
  ( long "after"
  <> short 'a'
  <> help "Take only commands after given time. Format: <num>[smhd] or HH:MM:SS"

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

