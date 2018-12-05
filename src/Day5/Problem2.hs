module Day5.Problem2 where

import qualified Day5.Problem1                 as P1
import           Data.Char
import           Util.IO
import           Data.List
import           Data.Ord

main :: IO ()
main = do
        input <- takeWhile (not . isSpace) <$> getInput 5
        let removedGroups = map
                    (\letter -> filter ((/= letter) . toLower) input)
                    ['a' .. 'z']
        printWithTime . minimum $ map (length . P1.removeReverses) removedGroups
