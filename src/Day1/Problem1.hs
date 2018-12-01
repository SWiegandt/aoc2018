{-# LANGUAGE OverloadedStrings #-}

module Day1.Problem1 where

import qualified Data.Text                     as T
import           Util.IO

parseFrequency :: T.Text -> Int
parseFrequency t =
    let freq = read . T.unpack . T.tail $ t
    in  case (T.head t) of
            '+' -> freq
            '-' -> -1 * freq

main :: IO ()
main = do
    input <- T.pack
        <$> readFile "/home/sebastian/git/aoc2018/src/Day1/Problem1.input"
    let frequencies = filter (not . T.null) $ T.splitOn "\n" input
    print . sum . fmap parseFrequency $ frequencies
