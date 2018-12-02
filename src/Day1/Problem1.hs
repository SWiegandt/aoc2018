module Day1.Problem1 where

import           Util.IO

parseFrequency :: String -> Int
parseFrequency ('+' : f) = read f
parseFrequency ('-' : f) = (-1) * read f

getFrequencies :: IO [Int]
getFrequencies = map parseFrequency . filter (/= "") . lines <$> getInput 1

main :: IO ()
main = getFrequencies >>= print . sum
