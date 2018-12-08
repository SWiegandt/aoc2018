module Day8.Problem2 where

import qualified Day8.Problem1                 as P1
import           Data.Tree
import           Util.IO
import           Data.Maybe
import           Safe

getValue :: Tree [Int] -> Int
getValue (Node metadata []) = sum metadata
getValue (Node metadata children) =
    let valueChildren = mapMaybe ((children `atMay`) . subtract 1) metadata
    in  if null valueChildren then 0 else sum . map getValue $ valueChildren

main :: IO ()
main = P1.getTree >>= printWithTime . getValue
