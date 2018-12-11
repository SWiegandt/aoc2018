{-# LANGUAGE TupleSections #-}
module Day11.Problem1 where

import           Util.IO
import           Data.List
import           Data.Function
import           Safe                          as S
import           Data.Maybe
import           Data.Ord
import           Data.Array.IArray             as A

type Gridsize = Int
type Energy = Int
type Coordinate = (Int, Int)

energy :: Coordinate -> Energy
energy (x, y) =
    let serial = 9445
        rack   = (x + 10)
        t      = rack * (rack * y + serial)
    in  t `quot` 100 `mod` 10 - 5

toInt :: Coordinate -> Int
toInt (x, y) = (x - 1) + 300 * (y - 1)

energyGrid :: A.Array Int Energy
energyGrid =
    A.array (toInt (1, 1), toInt (300, 300))
        $ ((,) <$> [1 .. 300] <*> [1 .. 300])
        & map (\c -> (toInt c, energy c))

subGridLines :: Coordinate -> Gridsize -> [Energy]
subGridLines (x, y) g =
    let constX = [ toInt (x + g - 1, y') | y' <- [y .. y + g - 1] ]
        constY = [ toInt (x', y + g - 1) | x' <- [x .. x + g - 2] ]
    in  map (energyGrid A.!) constX ++ map (energyGrid A.!) constY

subGridEnergies :: Coordinate -> [Energy]
subGridEnergies (x, y) =
    scanl' (+) 0 (map (sum . subGridLines (x, y)) [1 .. 300 - max x y + 1])

main :: IO ()
main =
    printWithTime
        .   fst
        .   maximumBy (comparing snd)
        .   mapMaybe (\(c, es) -> (c, ) <$> (es `S.atMay` 3))
        $   (\x y -> ((x, y), subGridEnergies (x, y)))
        <$> [1 .. 300]
        <*> [1 .. 300]
