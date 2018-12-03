module Day3.Problem2 where

import qualified Day3.Problem1                 as P1
import qualified Data.Map                      as M
import qualified Data.Set                      as S

main :: IO ()
main = do
        sheet <- M.elems . P1.makeSheet <$> P1.getPatches
        let all         = S.fromList . concat $ sheet
        let overlapping = S.fromList . concat . filter ((> 1) . length) $ sheet
        print . head . S.toList $ all S.\\ overlapping
