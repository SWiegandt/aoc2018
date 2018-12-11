module Day11.Problem2 where

import           Util.IO
import           Data.Ord
import           Data.List
import           Day11.Problem1                as P1

main :: IO ()
main =
  printWithTime
    .   (\((x, y), es) -> (x, y, fst . maximumBy (comparing snd) $ es))
    .   maximumBy (comparing (maximum . map snd . snd))
    $   (\x y -> ((x, y), [0 ..] `zip` P1.subGridEnergies (x, y)))
    <$> [1 .. 300]
    <*> [1 .. 300]
