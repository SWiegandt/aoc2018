module Util.Map where

import qualified Data.Map                      as M

increaseByOne :: Ord k => k -> M.Map k Int -> M.Map k Int
increaseByOne = M.alter (Just . maybe 1 (+ 1))
