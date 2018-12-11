module Day11.Problem1 where

import           Util.IO
import           Data.Matrix
import qualified Data.Map                      as M
import           Data.List

energy :: (Int, Int) -> Int
energy (y, x) =
    let serial = 18
        rack   = (x + 10)
        t      = rack * (rack * y + serial)
    in  t `quot` 100 `mod` 10 - 5

calculateEnergyGrid :: Int -> [(Int, (Int, Int))]
calculateEnergyGrid g = do
    let size  = 300
    let input = matrix size size energy
    let i     = identity size
    x <- [1 .. size - g]
    y <- [1 .. size - g]
    let subInput = submatrix y (y + g - 1) x (x + g - 1) input
    return (sum subInput, (x, y))

getMaxGrid :: Int -> (Int, (Int, Int))
getMaxGrid g = foldl1' (\a'@(s', _) a@(s, _) -> if s > s' then a else a')
                       (calculateEnergyGrid g)

main :: IO ()
main = printWithTime $ getMaxGrid 3
