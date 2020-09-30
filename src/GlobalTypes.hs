module GlobalTypes
    ( Options (..)
    , RelTimeFilter (..)
    , TimeFilterUnit (..)
    ) where

data TimeFilterUnit = Seconds | Minutes | Hours | Days
data RelTimeFilter = RelTimeFilter
  { number :: Int
  , unit   :: TimeFilterUnit }

data Options = Options
  { sessionSeparator :: String
  , current :: Bool
  , timeFilters :: [RelTimeFilter] }
