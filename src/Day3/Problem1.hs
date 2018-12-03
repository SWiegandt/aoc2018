{-# LANGUAGE OverloadedStrings #-}
module Day3.Problem1 where

import           Util.IO
import           Util.Map
import qualified Data.Text                     as T
import           Data.Char
import qualified Data.Map                      as M
import           Control.Monad

data Patch = Patch { patchId :: Int, left :: Int, top :: Int, width :: Int, height :: Int } deriving Show

parsePatch :: String -> Patch
parsePatch s =
    let parts = map T.unpack . T.splitOn " " . T.pack $ s
        coordinates =
            map (read . T.unpack)
                .  T.splitOn ","
                .  T.pack
                .  takeWhile (/= ':')
                $  parts
                !! 2
        size = map (read . T.unpack) . T.splitOn "x" . T.pack $ parts !! 3
        id   = read . takeWhile isDigit . tail $ s
    in  Patch id (head coordinates) (last coordinates) (head size) (last size)

updateSheet :: Patch -> M.Map (Int, Int) [Int] -> M.Map (Int, Int) [Int]
updateSheet patch sheet =
    let coordinates =
            [ (x + left patch, y + top patch)
            | x <- [0 .. width patch - 1]
            , y <- [0 .. height patch - 1]
            ]
    in  foldr (M.alter (Just . maybe [patchId patch] (patchId patch :)))
              sheet
              coordinates

getPatches :: IO [Patch]
getPatches = map parsePatch . lines <$> getInput 3

makeSheet :: [Patch] -> M.Map (Int, Int) [Int]
makeSheet = foldr updateSheet M.empty

main :: IO ()
main = M.size . M.filter ((> 1) . length) . makeSheet <$> getPatches >>= print
