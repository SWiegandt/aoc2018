module Util.IO where

import           System.IO

prompt :: String -> IO String
prompt query = do
    putStr query
    hFlush stdout
    getLine

getInput :: Int -> IO String
getInput day =
    let day' = show day
    in  readFile
        $  "/Users/sebastian.wiegandt/github/aoc2018/src/Day"
        ++ day'
        ++ "/Day"
        ++ day'
        ++ ".input"
