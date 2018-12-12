module Day12.Day12 where

import           Util.IO
import           Control.Monad.Trans.State
import           Data.List
import           Control.Arrow
import           Safe.Exact
import           Data.Maybe
import qualified Data.Map.Strict               as M

type Pot = Char
type Rules = M.Map [Pot] Bool

parseInput :: IO ([Pot], Rules)
parseInput = do
    input <- filter (not . null) . lines <$> getInput 12
    return
        (last . words . head $ input, M.fromList $ map parseRule (tail input))

parseRule :: String -> ([Pot], Bool)
parseRule s = let [r, _, b : _] = words s in (r, b == '#')

expandRow :: State ([Pot], Int) ()
expandRow = modify $ \(ps, n) -> ("....." ++ ps ++ ".....", n - 5)

applyRules :: Rules -> State ([Pot], Int) ()
applyRules rules = modify $ \(pots, n) ->
    let groups = mapMaybe (takeExactMay 5) . tails $ pots
    in  (map (\g -> if rules M.! g then '#' else '.') $! groups, n + 2)

dropEmpty :: State ([Pot], Int) ()
dropEmpty = do
    (empty, rest) <-
        second (reverse . dropWhile (== '.') . reverse)
        .   span (== '.')
        .   fst
        <$> get
    modify $ \(_, n) -> (rest, n + length empty)

runGame :: Rules -> Int -> State ([Pot], Int) Int
runGame rules 0 =
    sum
        .   map snd
        .   filter ((== '#') . fst)
        .   (\(pots, n) -> zip pots (map (+ n) [0 ..]))
        <$> get
runGame rules n =
    expandRow >> applyRules rules >> dropEmpty >> runGame rules (n - 1)

main :: IO ()
main = do
    (ns, rules) <- parseInput
    printWithTime $ evalState (runGame rules 50000) (ns, 0)
