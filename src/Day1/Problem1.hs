{-# LANGUAGE OverloadedStrings #-}

module Day1.Problem1 where

import qualified Data.Text                     as T
import           Util.IO

parseFrequency :: T.Text -> Int
parseFrequency t =
    let freq = read . T.unpack . T.tail $ t
    in  case T.head t of
            '+' -> freq
            '-' -> -1 * freq

getFrequencies :: IO [Int]
getFrequencies = do
    input <- T.pack
        <$> readFile "/home/sebastian/git/aoc2018/src/Day1/Day1.input"
    let nonEmptyFreqs = filter (not . T.null) $ T.splitOn "\n" input
    return $ fmap parseFrequency nonEmptyFreqs

main :: IO ()
main = getFrequencies >>= print . sum
