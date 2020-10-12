module Routines.FilterByTime
    ( filterByTimeIO
    ) where

import Data.Time ( getCurrentTime, UTCTime, addUTCTime, NominalDiffTime )
import GlobalTypes

toSeconds :: Int -> TimeFilterUnit -> Int
toSeconds n Seconds = n
toSeconds n Minutes = n * 60
toSeconds n Hours   = n * 3600
toSeconds n Days    = n * 86400

translate :: UTCTime -> TimeFilter -> UTCTime
translate now (AbsTimeFilter h m s) = 
translate now (RelTimeFilter n u) = let
  pastSeconds :: NominalDiffTime
  pastSeconds = fromIntegral $ negate $ abs $ toSeconds n u
  pastTime = addUTCTime pastSeconds now
  in if n > 0
       then After  pastTime
       else Before pastTime

filterByTimeIO :: String -> Bool -> TimeFilter -> TimeFilter -> IO ()
filterByTimeIO sesDel currOnly after before = do
  now <- getCurrentTime
  let absAfter = translate after
  let absBefore = translate before
  lns <- fmap lines getContents
  return ()

