module Day7.Problem1 where

import           Util.IO
import           Data.Ord
import qualified Data.List                     as L
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Loops
import           Data.List.Safe

newtype Step = Step { name :: Char } deriving (Show, Eq)
data Order = Order { from :: Step, to :: Step } deriving (Show, Eq)
data Worker = Worker { secondsLeft :: Int, step :: Step } deriving Show

parseOrder :: String -> Order
parseOrder s =
    let [_, step : _, _, _, _, _, _, step' : _, _, _] = words s
    in  Order (Step step) (Step step')

isTransitive :: [Order] -> Step -> Step -> Bool
isTransitive orders s1 s2 =
    let orders' = filter ((== s1) . from) orders
    in  Order s1 s2 `elem` orders || any (\s -> isTransitive orders s s2)
                                         (map to orders')

isLargestAvailable :: [Order] -> [Step] -> Step -> Bool
isLargestAvailable orders steps step =
    all (not . (\s -> isTransitive orders s step)) steps

awaitingWork :: [Order] -> [Worker] -> Step -> Bool
awaitingWork orders workers s =
    any (\s' -> isTransitive orders s' s) (map step workers)

extractMinimum :: [Order] -> [Worker] -> State [Step] (Maybe Step)
extractMinimum orders workers = do
    steps <- get
    let minimum =
            minimumBy (comparing name)
            . filter (isLargestAvailable orders steps)
            . filter (not . awaitingWork orders workers)
            $ steps :: Maybe Step
    put $ filter (`notElem` minimum) steps
    return minimum

getOrders :: IO [Order]
getOrders = map parseOrder . filter (not . null) . lines <$> getInput 7

getSteps :: [Order] -> [Step]
getSteps input =
    L.nub $ map (\(Order s _) -> s) input ++ map (\(Order _ s) -> s) input

main :: IO ()
main = do
    -- Problem 1
    input <- getOrders
    let steps = getSteps input
    printWithTime
        . map (maybe '\0' name)
        . evalState (untilM (extractMinimum input []) (null <$> get))
        $ steps

    -- Problem 2
