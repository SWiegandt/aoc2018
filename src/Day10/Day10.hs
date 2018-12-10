module Day10.Day10 where

import           Util.IO
import qualified Day10.Problem1                as P1
import qualified Day10.Problem2                as P2

main :: IO ()
main = do
    problem <- read <$> prompt "Enter problem: " :: IO Int

    case problem of
        1 -> P1.main
        2 -> P2.main
        _ -> error "Problem not yet implemented!"
