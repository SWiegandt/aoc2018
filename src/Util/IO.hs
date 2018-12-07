module Util.IO where

import           System.IO
import           System.TimeIt
import           Text.Printf

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

printWithTime :: Show a => a -> IO ()
printWithTime a = do
    (time, _) <- timeItT (print a)
    printf "Result in %6.5fs\n" time
