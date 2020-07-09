module Main where

import System.IO ( stdin, hSetEncoding )
import System.Environment ( getArgs )
import Data.List ( foldl', isSuffixOf )
import GHC.IO.Encoding ( mkTextEncoding )
import Data.Time ( getCurrentTime, UTCTime, addUTCTime, NominalDiffTime )

-- arg parsing
import Options.Applicative
import Data.Semigroup ((<>))

data TimeFilterUnit = Seconds | Minutes | Hours | Days
data RelTimeFilter = RelTimeFilter
  { number :: Int
  , unit   :: TimeFilterUnit }

data AbsTimeFilter = Before UTCTime
                   | After UTCTime

data Options = Options
  { sessionSeparator :: String
  , current :: Bool
  , timeFilters :: [RelTimeFilter] }

currentP :: Parser Bool
currentP = switch ( long "current" <> short 'c' <> help "Commands from current session only" )

sessionSepP :: Parser String
sessionSepP = strOption
  ( long "session-delim"
  <> help "Session separator string"
  <> showDefault
  <> value "### Session ###" )

timeFiltersP :: Parser RelTimeFilter
timeFiltersP = strOption
  ( long "time"
  <> short 't'
  <> help "Take only commands before/after given time. Format: [+-]<int>[smhd]" )

options :: Parser Options
options = Options <$> sessionSepP <*> currentP <*> many(timeFiltersP)

opts :: ParserInfo Options
opts = info (options <**> helper)
  (  fullDesc
  <> header "Extension to bash history builtin" )

run :: Options -> IO ()
run (Options _  False []) = interact id
run (Options ss True [])  = currSessIO ss
run (Options ss curr fs)  = filterByTimeIO ss curr fs

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

toSeconds :: Int -> TimeFilterUnit -> Int
toSeconds n Seconds = n
toSeconds n Minutes = n * 60
toSeconds n Hours   = n * 3600
toSeconds n Days    = n * 86400

translateTimeFilter :: UTCTime -> RelTimeFilter -> AbsTimeFilter
translateTimeFilter now (RelTimeFilter n u) = let
  pastSeconds :: NominalDiffTime
  pastSeconds = fromIntegral $ negate $ abs $ toSeconds n u
  pastTime = addUTCTime pastSeconds now
  in if n > 0
       then After  pastTime
       else Before pastTime

filterByTimeIO :: String -> Bool -> [RelTimeFilter] -> IO ()
filterByTimeIO sesDel currOnly timeFilters = do
  now <- getCurrentTime
  let absFilters = map (translateTimeFilter now) timeFilters
  lns <- fmap lines getContents
  return ()
