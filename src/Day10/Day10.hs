module Day10.Day10 where

import           Util.IO
import           Control.Monad
import           Data.Char
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

data Light = Light { position :: (Int, Int), velocity :: (Int, Int) }
newtype Sky = Sky { lights :: [Light] }

(<&>) = flip (<$>)

instance Show Sky where
    show (Sky lights) =
        let (cx, cy) = avgPos (Sky lights)
            height   = 10
            width    = 100
        in  unlines $ [cy - height .. cy + height] <&> \y ->
                [cx - width .. cx + width] <&> \x ->
                    if (x, y) `elem` map position lights then '#' else '.'

avgPos :: Sky -> (Int, Int)
avgPos (Sky sky) =
    let fsts = map (fst . position) sky
        snds = map (snd . position) sky
    in  (sum fsts `quot` length sky, sum snds `quot` length sky)

getSky :: IO Sky
getSky = Sky . map parseLight . filter (not . null) . lines <$> getInput 10

parseLight :: String -> Light
parseLight =
    (\[x, y, vx, vy] -> Light (x, y) (vx, vy))
        . map read
        . filter (all (\c -> isDigit c || c == '-'))
        . words
        . map (\c -> if c `elem` ",<>" then ' ' else c)

moveLight :: Light -> Light
moveLight (Light (x, y) v@(vx, vy)) = Light (x + vx, y + vy) v

inBoundingBox :: [Light] -> Light -> Bool
inBoundingBox lights (Light (x, y) _) =
    let xMax = maximum . map (fst . position) $ lights
        yMax = maximum . map (snd . position) $ lights
    in  abs (x - xMax) < 200 && abs (y - yMax) < 50 -- UGLY :'(

movingSky :: Int -> StateT Sky IO ()
movingSky n = do
    modify (Sky . map moveLight . lights)
    sky@(Sky lights) <- get
    when (all (inBoundingBox lights) lights) $ liftIO (print sky >> print n)

main :: IO ()
main = do
    sky <- getSky
    evalStateT (mapM_ movingSky [1 ..]) sky
