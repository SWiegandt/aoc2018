module Main where

import           Util.IO
import qualified Day1.Day1                     as Day1

main :: IO ()
main = do
    day <- read <$> prompt "Enter day: " :: IO Int

    case day of
        1 -> Day1.main
        _ -> error "Day not yet implemented!"
