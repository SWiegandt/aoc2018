{-# LANGUAGE TupleSections #-}
module Day18.Day18 where

import           Util.IO
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Loops
import           Data.List

type Coordinate = (Int, Int)
data Acre = Ground | Tree | Lumberyard deriving (Show, Eq)
type Plot = M.Map Coordinate Acre

parseAcre :: Char -> Acre
parseAcre '.' = Ground
parseAcre '|' = Tree
parseAcre '#' = Lumberyard

parsePlot :: String -> Plot
parsePlot =
    M.fromList
        . map (\(pos, c) -> (pos, ) . parseAcre $ c)
        . concatMap (uncurry (map . first . flip (,)) . second ([0 ..] `zip`))
        . ([0 ..] `zip`)
        . lines

neighborhood :: Coordinate -> [Coordinate]
neighborhood (x, y) =
    filter (/= (x, y))
        $   (\x' y' -> (x + x', y + y'))
        <$> [-1 .. 1]
        <*> [-1 .. 1]

growAcre :: Plot -> Coordinate -> Acre -> Acre
growAcre plot pos acre =
    let neighbors   = mapMaybe (plot M.!?) $ neighborhood pos
        grounds     = filter (== Ground) neighbors
        trees       = filter (== Tree) neighbors
        lumberyards = filter (== Lumberyard) neighbors
    in  case acre of
            Ground     -> if length trees >= 3 then Tree else acre
            Tree       -> if length lumberyards >= 3 then Lumberyard else acre
            Lumberyard -> if not (null lumberyards) && not (null trees)
                then acre
                else Ground

growPlot :: Plot -> Plot
growPlot plot = M.mapWithKey (growAcre plot) plot

printPlot :: Plot -> IO ()
printPlot plot = do
    let trees       = M.size . M.filter (== Tree) $ plot
    let lumberyards = M.size . M.filter (== Lumberyard) $ plot
    print $ trees * lumberyards

findRepeat :: Int -> Plot -> [Plot] -> Maybe (Int, Int)
findRepeat iteration plot plots
    | plot `elem` plots = (\n -> (iteration, n + 1)) <$> elemIndex plot plots
    | otherwise = findRepeat (iteration + 1) (growPlot plot) (plot : plots)

main :: IO ()
main = do
    input <- parsePlot <$> getInput 18
    let iterations = iterate growPlot input

    -- part 1
    printPlot $ iterations !! 10

    -- part 2
    let Just (firstRepeat, period) = findRepeat 0 input []
    printPlot
        $  iterations
        !! (firstRepeat + ((1000000000 - firstRepeat) `mod` period))
