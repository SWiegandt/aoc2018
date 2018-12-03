module Day3.Problem2 where

import qualified Day3.Problem1                 as P1
import qualified Data.Map                      as M
import qualified Data.Set                      as S

main :: IO ()
main = do
    sheet <- P1.makeSheet <$> P1.getPatches
    let overlapping =
            S.fromList . concat . M.elems . M.filter ((> 1) . length) $ sheet
    let notOverlapping =
            S.fromList . concat . M.elems . M.filter ((== 1) . length) $ sheet
    print (S.difference notOverlapping overlapping)
