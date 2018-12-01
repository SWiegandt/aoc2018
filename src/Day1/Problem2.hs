module Day1.Problem2 where

import qualified Day1.Problem1                 as P1
import           Control.Monad.Trans.State.Strict
import qualified Data.Set                      as S

findRepeated :: [Int] -> Int -> State (S.Set Int) Int
findRepeated (freq : freqs) sum = do
    let sum' = sum + freq
    visited <- get

    if sum' `S.member` visited
        then return sum'
        else do
            put (S.insert sum' visited)
            findRepeated freqs sum'

main :: IO ()
main = do
    frequencies <- cycle <$> P1.getFrequencies
    let repeated = evalState (findRepeated frequencies 0) (S.singleton 0)
    print repeated
