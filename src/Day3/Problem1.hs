{-# LANGUAGE RecordWildCards #-}

module Day3.Problem1 where

import           Util.IO
import qualified Data.List.Split               as S
import qualified Data.Map.Strict               as M

data Patch = Patch { patchId :: Int, left :: Int, top :: Int, width :: Int, height :: Int } deriving Show

parsePatch :: String -> Patch
parsePatch s =
    let (patchId : left : top : width : height : _) =
            map read . filter (not . null) . S.splitOneOf "#@,:x " $ s
    in  Patch { .. }

updateSheet :: Patch -> M.Map (Int, Int) [Int] -> M.Map (Int, Int) [Int]
updateSheet Patch {..} sheet =
    let coordinates =
            [ (x + left, y + top)
            | x <- [0 .. width - 1]
            , y <- [0 .. height - 1]
            ]
    in  foldr (\k -> M.insertWith (++) k [patchId]) sheet coordinates

getPatches :: IO [Patch]
getPatches = map parsePatch . lines <$> getInput 3

makeSheet :: [Patch] -> M.Map (Int, Int) [Int]
makeSheet = foldr updateSheet M.empty

main :: IO ()
main = getPatches >>= printWithTime . M.size . M.filter ((> 1) . length) . makeSheet
