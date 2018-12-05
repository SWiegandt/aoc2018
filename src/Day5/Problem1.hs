module Day5.Problem1 where

import           Data.Char
import           Util.IO

isReverse :: Char -> Char -> Bool
isReverse a b = a /= b && toLower a == toLower b

removeReverse :: Char -> String -> String
removeReverse c ""        = [c]
removeReverse c (c' : cs) = if isReverse c c' then cs else c : c' : cs

removeReverses :: String -> String
removeReverses = foldr removeReverse ""

main :: IO ()
main = getInput 5 >>= print . length . removeReverses . takeWhile (not . isSpace)
