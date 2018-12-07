{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Day4.Problem1 where

import           Util.IO
import           Data.List.Split
import           Data.List
import           Control.Monad
import qualified Data.Map                      as M
import           Control.Monad.Trans.State.Strict
import           Data.Ord
import           Data.Maybe

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

readEvent
    :: (GuardState, M.Map Int [Int])
    -> GuardEvent
    -> (GuardState, M.Map Int [Int])
readEvent (GuardState guard m, map) GuardEvent {..} = case event of
    Start g    -> (GuardState g minute, map)
    FallAsleep -> (GuardState guard minute, map)
    WakeUp ->
        let minutes = [m .. minute - 1]
        in  ( GuardState guard minute
            , foldr (\n -> M.insertWith (++) guard [n]) map minutes
            )

getGuard :: Event -> Maybe Int
getGuard (Start n) = Just n
getGuard _         = Nothing

sleeperMap :: IO [(Int, [Int])]
sleeperMap = do
    input <- map parseEvent . sort . lines <$> getInput 4
    return
        . M.toList
        . snd
        . foldl' readEvent (GuardState (-1) (-1), M.empty)
        $ input

main :: IO ()
main = do
    (guard, sleepList) <- maximumBy (comparing $ length . snd) <$> sleeperMap
    let mostSleptMinute =
            head . maximumBy (comparing length) $ group (sort sleepList)
    printWithTime $ guard * mostSleptMinute
