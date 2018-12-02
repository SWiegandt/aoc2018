module Day2.Problem2 where

import           Util.IO

findDuplicate :: [String] -> (String, String)
findDuplicate (search : others) =
    let similar = dropWhile
            ((> 1) . length . filter not . zipWith (==) search)
            others
    in  case similar of
            []        -> findDuplicate others
            (res : _) -> (search, res)

main :: IO ()
main = do
    input <- lines <$> getInput 2
    print
        . map fst
        . filter (uncurry (==))
        . uncurry zip
        . findDuplicate
        $ input
