module Day2.Problem1 where

import           Util.IO
import           Control.Arrow
import qualified Data.Map.Strict               as M
import           Control.Applicative

numberOfInstances :: String -> M.Map Char Int
numberOfInstances = foldr (M.alter (Just . maybe 1 (+ 1))) M.empty

main :: IO ()
main = do
    instances <- map (M.elems . numberOfInstances) . lines <$> getInput 2
    print . product . map (\n -> length (filter (n `elem`) instances)) $ [2, 3]
