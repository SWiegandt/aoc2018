module Day12.Problem1 where

import           Util.IO
import           Control.Monad.Trans.State
import           Control.Applicative
import           Data.List
import           Debug.Trace

type Pot = Bool
type Neighborhood = (Pot, Pot, Pot, Pot, Pot)
type Rule = Neighborhood -> Pot

appendEmpty :: State [Pot] ()
appendEmpty = undefined

parseInput :: IO ([Neighborhood], [Rule])
parseInput = do
    input <- filter (not . null) . lines <$> getInput 12
    return (parsePots (head input), map parseRule (tail input))

parsePots :: String -> [Neighborhood]
parsePots s =
    let pots = map (== '#') $ ".." ++ last (words s) ++ ".."
        t    = map ZipList $ tails pots
    in  getZipList
        $   (,,,,)
        <$> ZipList pots
        <*> (t !! 1)
        <*> (t !! 2)
        <*> (t !! 3)
        <*> (t !! 4)

parseRule :: String -> Rule
parseRule = undefined

main :: IO ()
main = parseInput >>= print . fst
