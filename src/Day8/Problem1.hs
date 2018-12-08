module Day8.Problem1 where

import           Util.IO
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Tree

getHeader :: State [Int] (Int, Int)
getHeader = state $ \(children : metas : ls) -> ((children, metas), ls)

getMetadata :: Int -> State [Int] [Int]
getMetadata n = state $ \ls -> splitAt n ls

parseTree :: State [Int] (Tree [Int])
parseTree = do
    (children, metas) <- getHeader
    childTrees        <- replicateM children parseTree
    metadata          <- getMetadata metas
    return $ Node metadata childTrees

getTree :: IO (Tree [Int])
getTree =
    evalState parseTree . map read . filter (not . null) . words <$> getInput 8

main :: IO ()
main =
    getTree >>= printWithTime . foldTree (\metas sums -> sum sums + sum metas)

