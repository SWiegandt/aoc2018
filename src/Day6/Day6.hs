module Day6.Day6 where

import           Util.IO
import           Data.List.Split
import           Data.List
import qualified Data.Map                      as M
import           Data.Ord
import           Control.Monad

type InputCoordinate = (Int, Int)
type GridCoordinate = (Int, Int)

parseCoordinates :: String -> InputCoordinate
parseCoordinates s =
    let [x, y] = filter (not . null) . splitOneOf ", " $ s in (read x, read y)

fillGrid
    :: [InputCoordinate]
    -> [GridCoordinate]
    -> M.Map GridCoordinate [InputCoordinate]
fillGrid input = foldr (\k -> M.insert k (findClosest input k)) M.empty

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

findClosest :: [InputCoordinate] -> GridCoordinate -> [InputCoordinate]
findClosest input coord =
    let minimum =
            manhattan coord $ minimumBy (comparing $ manhattan coord) input
    in  filter ((== minimum) . manhattan coord) input

calculateDistances
    :: [InputCoordinate] -> [GridCoordinate] -> M.Map GridCoordinate [Int]
calculateDistances input =
    foldr (\k -> M.insert k (map (manhattan k) input)) M.empty

main :: IO ()
main = do
    input <- map parseCoordinates . filter (not . null) . lines <$> getInput 6
    let xs = map fst input
    let ys = map snd input
    let grid =
            [ (x, y)
            | x <- [minimum xs .. maximum xs]
            , y <- [minimum ys .. maximum ys]
            ]

    -- Problem 1
    let filledGrid = M.filter ((== 1) . length) $ fillGrid input grid
    let onEdge = join . M.elems $ M.filterWithKey
            (\(x, y) _ ->
                (x `elem` [minimum xs, maximum xs])
                    || (y `elem` [minimum ys, maximum ys])
            )
            filledGrid
    let nonInfinite = M.filter (all (`notElem` onEdge)) filledGrid
    printWithTime
        . length
        . maximumBy (comparing length)
        . group
        . sort
        . join
        . M.elems
        $ nonInfinite

    -- Problem 2
    printWithTime . M.size . M.filter ((< 10000) . sum) $ calculateDistances
        input
        grid
