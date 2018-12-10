module Day10.Problem1 where

import           Util.IO
import           Control.Monad
import           Data.Char

data Light = Light { position :: (Int, Int), velocity :: (Int, Int) }
newtype Sky = Sky [Light]

instance Show Sky where
    show sky = let avg = avgPos sky in boundingBox avg 50 15 sky

avgPos :: Sky -> (Int, Int)
avgPos (Sky sky) =
    let fsts = map (fst . position) sky
        snds = map (snd . position) sky
    in  (sum fsts `quot` length sky, sum snds `quot` length sky)

boundingBox :: (Int, Int) -> Int -> Int -> Sky -> String
boundingBox (cx, cy) width height (Sky sky) = do
    y <- [cy - height .. cy + height]
    let row = do
            x <- [cx - width .. cx + width]
            if (x, y) `elem` map position sky then return '#' else return '.'
    row ++ ['\n']

getSky :: IO Sky
getSky = Sky . map parseLight . filter (not . null) . lines <$> getInput 10

parseLight :: String -> Light
parseLight =
    (\[x, y, vx, vy] -> Light (x, y) (vx, vy))
        . map read
        . filter (all (\c -> isDigit c || c == '-'))
        . words
        . map (\c -> if c `elem` ",<>" then ' ' else c)

main :: IO ()
main = getSky >>= print
