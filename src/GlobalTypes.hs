module GlobalTypes
    ( Options (..)
    , TimeFilter (..)
    , TimeFilterUnit (..)
    ) where

import Data.Time ( UTCTime )

data TimeFilterUnit = Seconds | Minutes | Hours | Days

data TimeFilter =
    RelTimeFilter { number :: Int, unit :: TimeFilterUnit }
  | AbsTimeFilter { hours :: Int, minutes :: Int, seconds :: Int }

data Options = Options
  { sessionSeparator :: String
  , current :: Bool
  , after :: Maybe TimeFilter
  , before :: Maybe TimeFilter }

