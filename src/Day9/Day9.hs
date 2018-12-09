{-# LANGUAGE RecordWildCards #-}
module Day9.Day9 where

import           Data.List                     as L
import           Util.IO
import           Data.Sequence                 as S
import           Data.Char
import qualified Data.Map                      as M

type Player = Int
type Marble = Int
data GameBoard = GameBoard { board :: !(Seq Marble), scores :: M.Map Player [Marble], active :: Marble } deriving Show

getGame :: IO (Int, Int)
getGame =
    (\[a, b] -> (a, b)) . map read . L.filter (all isDigit) . words <$> getInput
        9

playRound :: GameBoard -> (Player, Marble) -> GameBoard
playRound b@GameBoard {..} (player, marble) = case marble `mod` 23 of
    0 -> score b (player, marble)
    _ ->
        let active' = (active + 2) `mod` S.length board
            board'  = insertAt active' marble board
        in  GameBoard board' scores active'

score :: GameBoard -> (Player, Marble) -> GameBoard
score GameBoard {..} (player, marble) =
    let active'           = (active - 7) `mod` S.length board
        (h, scored :<| t) = S.splitAt active' board
        scores'           = M.insertWith (++) player [marble, scored] scores
    in  GameBoard (h >< t) scores' active'

main :: IO ()
main = do
    (players, marbles) <- getGame
    let gamePlan = cycle [1 .. players] `L.zip` [1 .. marbles]
    let finishedGame =
            foldl' playRound (GameBoard (S.singleton 0) M.empty 0) gamePlan
    let maxScore = maximum . M.elems . M.map sum . scores $ finishedGame
    printWithTime maxScore
