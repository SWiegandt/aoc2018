module Day7.Day7 where

import           Util.IO
import qualified Day7.Problem1                 as P1

main :: IO ()
main = do
    problem <- read <$> prompt "Enter problem: " :: IO Int

    case problem of
        1 -> P1.main
        _ -> error "Problem not yet implemented!"
