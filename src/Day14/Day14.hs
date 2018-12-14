module Day14.Day14 where

import           Util.IO
import           Data.Char
import qualified Data.Sequence                 as S
import           Data.Maybe
import           Data.List

type Elves = [Int]

createRecipe :: (S.Seq Int, Elves) -> (S.Seq Int, Elves)
createRecipe (recipes, elves) =
    let elfRecipes     = mapMaybe (recipes S.!?) elves
        createdRecipes = map digitToInt . show . sum $ elfRecipes
        recipes'       = foldl' (S.|>) recipes createdRecipes
    in  ( recipes'
        , zipWith (\e r -> (e + r + 1) `mod` S.length recipes') elves elfRecipes
        )

extractFollowing :: Int -> [(S.Seq Int, Elves)] -> String
extractFollowing input =
    concatMap show . S.take 10 . S.drop input . fst . head . dropWhile
        (\(seq, _) -> S.length seq < (input + 10))

firstInstance :: Int -> [(S.Seq Int, Elves)] -> Int
firstInstance input =
    let input' = map digitToInt . reverse . show $ input
    in  subtract (length input')
        . S.length
        . (\s@(s' S.:|> _) -> if s' `endsWith` input' then s' else s)
        . head
        . dropWhile
              (\s@(s' S.:|> _) -> not . any (`endsWith` input') $ [s, s'])
        . map fst

endsWith :: S.Seq Int -> [Int] -> Bool
endsWith (s' S.:|> n') (n : ns) = n == n' && endsWith s' ns
endsWith _             []       = True
endsWith _             _        = False

main :: IO ()
main = do
    let input   = 440231
    let init    = (S.fromList [3, 7], [0 .. 1])
    let recipes = iterate createRecipe init

    -- part 1
    printWithTime . extractFollowing input $ recipes

    -- part 2
    printWithTime . firstInstance input $ recipes
