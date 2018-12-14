module Day14.Day14 where

import           Util.IO
import           Data.Char
import           Debug.Trace
import qualified Data.Map.Strict               as M

createRecipe :: (M.Map Int Int, [Int]) -> (M.Map Int Int, [Int])
createRecipe (recipes, elves) =
    let elfRecipes     = map (recipes M.!) elves
        createdRecipes = map digitToInt . show . sum $ elfRecipes
        recipes'       = foldr (\(n, r) -> M.insert n r) recipes
            $ zip [M.size recipes ..] createdRecipes
    in  ( recipes'
        , zipWith (\e r -> (e + r + 1) `mod` M.size recipes') elves elfRecipes
        )

main :: IO ()
main = do
    let input = 440231

    -- part 1
    printWithTime
        . concatMap show
        . take 10
        . drop input
        . map snd
        . M.toList
        . fst
        . head
        . dropWhile ((< (input + 10)) . M.size . fst)
        . iterate createRecipe
        $ (M.fromList $ [0 ..] `zip` [3, 7], [0 .. 1])
