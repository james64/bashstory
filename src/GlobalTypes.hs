module GlobalTypes
    ( Options (..)
    , RelTimeFilter (..)
    , TimeFilterUnit (..)
    ) where

import Data.Time ( UTCTime )

data TimeFilterUnit = Seconds | Minutes | Hours | Days

data TimeFilter =
    RelTimeFilter { number :: Int, unit :: TimeFilterUnit }
  | AbsTimeFilter { timestamp :: UTCTime }

data Options = Options
  { sessionSeparator :: String
  , current :: Bool
  , before :: Maybe TimeFilter
  , after :: Maybe TimeFilter }

