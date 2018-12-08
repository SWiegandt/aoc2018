module Main where

import           Util.IO
import qualified Day1.Day1                     as Day1
import qualified Day2.Day2                     as Day2
import qualified Day3.Day3                     as Day3
import qualified Day4.Day4                     as Day4
import qualified Day5.Day5                     as Day5
import qualified Day6.Day6                     as Day6
import qualified Day7.Day7                     as Day7
import qualified Day8.Day8                     as Day8

main :: IO ()
main = do
    day <- read <$> prompt "Enter day: " :: IO Int

    case day of
        1 -> Day1.main
        2 -> Day2.main
        3 -> Day3.main
        4 -> Day4.main
        5 -> Day5.main
        6 -> Day6.main
        7 -> Day7.main
        8 -> Day8.main
        _ -> error "Day not yet implemented!"
