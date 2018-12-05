module Main where

import           Util.IO
import qualified Day1.Day1                     as Day1
import qualified Day2.Day2                     as Day2
import qualified Day3.Day3                     as Day3
import qualified Day4.Day4                     as Day4
import qualified Day5.Day5                     as Day5

main :: IO ()
main = do
    day <- read <$> prompt "Enter day: " :: IO Int

    case day of
        1 -> Day1.main
        2 -> Day2.main
        3 -> Day3.main
        4 -> Day4.main
        5 -> Day5.main
        _ -> error "Day not yet implemented!"
