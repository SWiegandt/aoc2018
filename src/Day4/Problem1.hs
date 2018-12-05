{-# LANGUAGE RecordWildCards #-}

module Day4.Problem1 where

import           Util.IO
import           Data.List.Split
import           Data.List
import           Control.Monad
import qualified Data.Map                      as M
import           Control.Monad.Trans.State.Strict
import           Data.Function

data Event = Start Int | FallAsleep | WakeUp deriving Show
data GuardEvent = GuardEvent { year :: Int, month :: Int, day :: Int, hour :: Int, minute :: Int, event :: Event } deriving Show
data GuardState = GuardState Int Int

parseEvent :: String -> GuardEvent
parseEvent s =
    let split = filter (not . null) . splitOneOf "[]-:# " $ s
    in  case split of
            [year, month, day, hour, minute, _, guard, _, _] -> GuardEvent
                (read year)
                (read month)
                (read day)
                (read hour)
                (read minute)
                (Start $ read guard)
            [year, month, day, hour, minute, "falls", _] -> GuardEvent
                (read year)
                (read month)
                (read day)
                (read hour)
                (read minute)
                FallAsleep
            [year, month, day, hour, minute, "wakes", _] -> GuardEvent
                (read year)
                (read month)
                (read day)
                (read hour)
                (read minute)
                WakeUp

readEvent :: GuardEvent -> State (GuardState, M.Map Int [Int]) ()
readEvent GuardEvent {..} = do
    (GuardState guard m, map) <- get
    case event of
        Start g    -> put (GuardState g minute, map)
        FallAsleep -> put (GuardState guard minute, map)
        WakeUp ->
            let minutes = [m .. minute - 1]
            in  put
                    ( GuardState guard minute
                    , foldr (\n -> M.insertWith (++) guard [n]) map minutes
                    )

sleeperMap :: IO [(Int, [Int])]
sleeperMap = do
    input <- map parseEvent . sort . lines <$> getInput 4
    return
        . M.toList
        . snd
        . execState (mapM_ readEvent input)
        $ (GuardState (-1) (-1), M.empty)

main :: IO ()
main = do
    (guard, sleepList) <- maximumBy (compare `on` length . snd) <$> sleeperMap
    let mostSleptMinute =
            head . maximumBy (compare `on` length) $ group (sort sleepList)
    printWithTime $ guard * mostSleptMinute
