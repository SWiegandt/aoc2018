module Day1.Problem2 where

import qualified Day1.Problem1                 as P1
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Loops
import qualified Data.Set                      as S

type Frequencies = [Int]
type Sum = Int
type FrequencyState = (S.Set Sum, Frequencies, Sum)

isRepeated :: FrequencyState -> Bool
isRepeated (visited, _, sum) = sum `S.member` visited

insertSum :: State FrequencyState ()
insertSum = modify $ \(visited, freq : freqs, sum) ->
    (S.insert sum visited, freqs, sum + freq)

findRepeated :: State FrequencyState ()
findRepeated = insertSum `untilM_` (isRepeated <$> get)

main :: IO ()
main = do
    frequencies <- cycle <$> P1.getFrequencies
    let (_, _, repeated) = execState findRepeated (S.empty, frequencies, 0)
    print repeated
