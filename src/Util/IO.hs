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
        $  "/home/sebastian/git/aoc2018/src/Day"
        ++ day'
        ++ "/Day"
        ++ day'
        ++ ".input"
