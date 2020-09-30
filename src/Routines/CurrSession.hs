module Routines.CurrSession
    ( currSessIO
    ) where

import Data.List ( foldl', isSuffixOf )

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

