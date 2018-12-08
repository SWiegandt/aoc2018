module Day1.Problem2 where

import qualified Day1.Problem1                 as P1
import qualified Data.Set                      as S
import           Util.IO

type Frequencies = [Int]
type Sum = Int
type FrequencyState = (S.Set Sum, Frequencies, Sum)

isRepeated :: FrequencyState -> Bool
isRepeated (visited, _, sum) = sum `S.member` visited

insertSum :: FrequencyState -> FrequencyState
insertSum (visited, freq : freqs, sum) =
    (S.insert sum visited, freqs, sum + freq)

main :: IO ()
main = do
    frequencies <- cycle <$> P1.getFrequencies
    let (_, _, repeated) : _ =
            dropWhile (not . isRepeated)
                . iterate insertSum
                $ (S.empty, frequencies, 0)
    printWithTime repeated
