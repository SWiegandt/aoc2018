module Day7.Day7 where

import           Util.IO
import           Data.Ord
import qualified Data.List                     as L
import           Control.Monad
import           Control.Monad.Trans.State.Strict
                                         hiding ( state )
import           Control.Monad.Loops
import           Data.List.Safe
import           Data.Maybe
import           Control.Arrow

newtype Step = Step { name :: Char } deriving (Show, Eq)
data Order = Order { from :: Step, to :: Step } deriving (Show, Eq)
newtype Worker = Worker { state :: Maybe WorkerState }
data WorkerState = Working { duration :: Int, step :: Step } deriving (Show, Eq)

parseOrder :: String -> Order
parseOrder s =
    let [_, step : _, _, _, _, _, _, step' : _, _, _] = words s
    in  Order (Step step) (Step step')

isTransitive :: [Order] -> Step -> Step -> Bool
isTransitive orders s1 s2 =
    let orders' = filter ((== s1) . from) orders
    in  Order s1 s2 `elem` orders || any (\s -> isTransitive orders s s2)
                                         (map to orders')

getStep :: Worker -> Maybe Step
getStep (Worker state) = step <$> state

isFirstAvailable :: [Order] -> [Step] -> Step -> Bool
isFirstAvailable orders steps step =
    all (not . (\s -> isTransitive orders s step)) steps

awaitingWork :: [Order] -> [Worker] -> Step -> Bool
awaitingWork orders workers s =
    any (\s' -> isTransitive orders s' s) (mapMaybe getStep workers)

extractMinimum :: [Order] -> [Worker] -> State ([Step], Int) (Maybe Step)
extractMinimum orders workers = do
    (steps, duration) <- get
    let minimum =
            minimumBy (comparing name)
                . filter (isFirstAvailable orders steps)
                . filter (not . awaitingWork orders workers)
                $ steps
    modify (first $ filter (`notElem` minimum))
    return minimum

startWork :: Step -> Worker
startWork s@(Step name) =
    let duration = maybe 0 (+ 61) $ L.elemIndex name ['A' .. 'Z']
    in  Worker (Just $ Working duration s)

doWork :: Worker -> Worker
doWork (Worker state) = Worker $ do
    Working duration step <- state
    if duration <= 1 then Nothing else Just $ Working (duration - 1) step

runWorkers :: [Order] -> [Worker] -> State ([Step], Int) Int
runWorkers orders workers = do
    (steps, duration) <- get
    let (working, idle) = partition (isJust . state) workers
    let doneWork        = map doWork workers
    case idle of
        []       -> modify (second (+ 1)) >> runWorkers orders doneWork
        (_ : ws) -> do
            todo <- extractMinimum orders working
            case todo of
                Nothing -> if null steps && null working
                    then return duration
                    else modify (second (+ 1)) >> runWorkers orders doneWork
                Just s -> runWorkers orders (startWork s : (ws ++ working))

getOrders :: IO [Order]
getOrders = map parseOrder . filter (not . null) . lines <$> getInput 7

getSteps :: [Order] -> [Step]
getSteps input =
    L.nub $ map (\(Order s _) -> s) input ++ map (\(Order _ s) -> s) input

main :: IO ()
main = do
    input <- getOrders
    let steps = getSteps input

    -- Problem 1
    printWithTime
        . map (maybe '\0' name)
        . evalState (untilM (extractMinimum input []) (null . fst <$> get))
        $ (steps, 0)

    -- Problem 2
    let workers = replicate 5 (Worker Nothing)
    printWithTime . evalState (runWorkers input workers) $ (steps, 0)
