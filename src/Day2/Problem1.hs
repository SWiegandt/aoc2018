module Day2.Problem1 where

import           Util.IO
import qualified Data.Map.Strict               as M

numberOfInstances :: String -> M.Map Char Int
numberOfInstances = foldr (M.alter (Just . maybe 1 (+ 1))) M.empty

main :: IO ()
main = do
    instances <- map (M.elems . numberOfInstances) . lines <$> getInput 2
    print . product . map (\n -> length (filter (n `elem`) instances)) $ [2, 3]
