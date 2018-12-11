module Day11.Problem2 where

import           Util.IO
import           Control.Arrow
import           Data.List
import qualified Day11.Problem1                as P1

main :: IO ()
main =
  printWithTime
    . foldl1' (\a'@((s', _), _) a@((s, _), _) -> if s > s' then a else a')
    $ map (P1.getMaxGrid &&& id) [1 .. 299]
