module Parsers
    ( opts
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

import GlobalTypes

parseRelTime :: ReadM RelTimeFilter
parseRelTime = eitherReader $ parse
    where parse :: String -> Either String RelTimeFilter
          parse s = Left "chyba"

currentP :: Parser Bool
currentP = switch ( long "current" <> short 'c' <> help "Commands from current session only" )

sessionSepP :: Parser String
sessionSepP = strOption
  ( long "session-delim"
  <> help "Session separator string"
  <> showDefault
  <> value "### Session ###" )

timeFiltersP :: Parser RelTimeFilter
timeFiltersP = option parseRelTime
  ( long "time"
  <> short 't'
  <> help "Take only commands before/after given time. Format: [+-]<int>[smhd]" )

options :: Parser Options
options = Options <$> sessionSepP <*> currentP <*> many(timeFiltersP)

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> header "Extension to bash history builtin" )

