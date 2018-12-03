{-# LANGUAGE TupleSections #-}

module Day2.Problem2 where

import           Util.IO
import           Data.List

findDuplicate :: [String] -> (String, String)
findDuplicate (search : others) =
    let similar =
            find ((<= 1) . length . filter id . zipWith (/=) search) others
    in  maybe (findDuplicate others) (search, ) similar

main :: IO ()
main = do
    input <- lines <$> getInput 2
    print
        . map fst
        . filter (uncurry (==))
        . uncurry zip
        . findDuplicate
        $ input
