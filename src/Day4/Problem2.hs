module Day4.Problem2 where

import qualified Day4.Problem1                 as P1
import           Data.Function
import           Data.List

main :: IO ()
main = do
    mostSleptMinute <-
        map
                (\(g, ls) ->
                    (g, maximumBy (compare `on` length) . group . sort $ ls)
                )
            <$> P1.sleeperMap
    let (guard, minute) =
            maximumBy (compare `on` (length . snd)) mostSleptMinute
    print $ guard * head minute
