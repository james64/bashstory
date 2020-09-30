module Routines.FilterByTime
    ( filterByTimeIO
    ) where

import Data.Time ( getCurrentTime, UTCTime, addUTCTime, NominalDiffTime )
import GlobalTypes

data AbsTimeFilter = Before UTCTime
                   | After UTCTime

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

