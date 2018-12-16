{-# LANGUAGE TupleSections #-}

module Day15.Day15 where

import           Util.IO
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Control.Arrow
import           Control.Monad.Trans.State.Strict
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Ord
import           Data.List
import           Debug.Trace
import           Data.Function

data Block = Empty | Wall deriving Eq
data UnitType = Goblin | Elf deriving Eq
data Unit = Unit { unitType :: UnitType, hp :: Int, unitId :: Id, power :: Int }
type Id = Coordinate
type Coordinate = (Int, Int)
type GameMap a = M.Map Coordinate a
data GameState = GameState { blocks :: GameMap Block, units :: GameMap Unit, rounds :: Int }

instance Show Block where
    show Empty = "."
    show Wall  = "#"

instance Show Unit where
    show (Unit Goblin hp _ _) = "G"
    show (Unit Elf    hp _ _) = "E"

instance Show GameState where
    show (GameState blocks units _) =
        let maxX = maximum . map (fst . fst) . M.toList $ blocks
            maxY = maximum . map (snd . fst) . M.toList $ blocks
            getPos pos = maybe (show $ blocks M.! pos) show (units M.!? pos)
            getRow y = concatMap (getPos . (, y)) [0 .. maxX] ++ "\n"
        in  concatMap getRow [0 .. maxY]

readingOrder :: Coordinate -> Coordinate -> Ordering
readingOrder = comparing snd <> comparing fst

parseBlock :: Int -> Coordinate -> Char -> Maybe Block
parseBlock _ _ '#' = Just Wall
parseBlock _ _ _   = Just Empty

parseUnit :: Int -> Coordinate -> Char -> Maybe Unit
parseUnit power unitId 'G' = Just (Unit Goblin 200 unitId 3)
parseUnit power unitId 'E' = Just (Unit Elf 200 unitId power)
parseUnit _     _      _   = Nothing

parseMap
    :: (Int -> Coordinate -> Char -> Maybe a)
    -> Int
    -> String
    -> M.Map Coordinate a
parseMap f power =
    M.fromList
        . mapMaybe (\(pos, c) -> (pos, ) <$> f power pos c)
        . concatMap (uncurry (map . first . flip (,)) . second ([0 ..] `zip`))
        . ([0 ..] `zip`)
        . lines

parseGame :: Int -> String -> GameState
parseGame power s =
    GameState (parseMap parseBlock power s) (parseMap parseUnit power s) 0

isEmpty :: GameState -> Coordinate -> Bool
isEmpty (GameState blocks units _) pos =
    let block = blocks M.! pos in block == Empty && isNothing (units M.!? pos)

isEnemy :: Unit -> Unit -> Bool
isEnemy unit unit' = unitType unit /= unitType unit'

decreaseHp :: Int -> Unit -> Unit
decreaseHp n unit = unit { hp = hp unit - n }

targets :: GameState -> Unit -> GameMap Unit
targets (GameState _ units _) unit = M.filter (isEnemy unit) units

isNeighbor :: Coordinate -> Coordinate -> Bool
isNeighbor (x, y) (x', y') =
    (x == x' || y == y') && (abs (x - x') + abs (y - y')) == 1

inRange :: Unit -> GameState -> [Coordinate]
inRange unit st@(GameState blocks _ _) =
    let positions = M.keysSet $ targets st unit
        neighbors (x, y) =
            S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    in  M.keys
        . M.filterWithKey
              (\pos _ ->
                  isEmpty st pos && not (S.disjoint (neighbors pos) positions)
              )
        $ blocks

reachableIn
    :: GameState -> Coordinate -> Coordinate -> (GameMap (Maybe Int), Maybe Int)
reachableIn st@(GameState blocks units _) from to =
    let distances =
            execState (go 0)
                . M.mapWithKey (\k _ -> if k == from then Just 0 else Nothing)
                . M.filterWithKey (\k _ -> isEmpty st k || k == from)
                $ blocks
    in  (distances, join $ distances M.!? to)
  where
    go d = do
        current   <- M.keys . M.filter (== Just d) <$> get
        neighbors <-
            filter (\k -> any (isNeighbor k) current)
            .   M.keys
            .   M.filter isNothing
            <$> get
        unless (null neighbors) $ do
            forM_ neighbors $ \pos -> modify (M.insert pos (Just (d + 1)))
            go (d + 1)

playRound :: State GameState Int
playRound = do
    GameState blocks units round <- get
    let sortedUnits = sortBy (readingOrder `on` fst) . M.toList $ units
    let aliveElves  = M.size . M.filter ((== Elf) . unitType) $ units
    enemiesAlive                    <- mapM (unitTurn False) sortedUnits

    gs@(GameState blocks' units' _) <- get
    when (and enemiesAlive) $ put (GameState blocks' units' (round + 1))
    let aliveElves' = M.size . M.filter ((== Elf) . unitType) $ units'
    -- traceShowM gs
    return (aliveElves - aliveElves')

isOver :: State GameState Bool
isOver = do
    noElves   <- M.null . M.filter ((== Elf) . unitType) . units <$> get
    noGoblins <- M.null . M.filter ((== Goblin) . unitType) . units <$> get
    return (noElves || noGoblins)

unitTurn :: Bool -> (Coordinate, Unit) -> State GameState Bool
unitTurn hasMoved (pos, unit) = do
    stillAlive <-
        (== Just (unitId unit)) . (M.!? pos) . M.map unitId . units <$> get

    if stillAlive
        then do
            enemyPositions <- M.keys . M.filter (isEnemy unit) . units <$> get
            if null enemyPositions
                then return False
                else if any (isNeighbor pos) enemyPositions
                    then unitAttack pos unit >> return True
                    else if not hasMoved
                        then do
                            enemyRanges <- inRange unit <$> get
                            newPos      <- unitMove pos enemyRanges unit
                            unitTurn True (newPos, unit)
                        else return True
        else return True

unitAttack :: Coordinate -> Unit -> State GameState ()
unitAttack pos unit = do
    GameState blocks units rounds <- get
    let enemiesInRange =
            M.filterWithKey (\k u -> isEnemy unit u && isNeighbor pos k) units
    let unitToAttack =
            minimumBy ((comparing hp `on` snd) <> (readingOrder `on` fst))
                . M.toList
                $ enemiesInRange
    put (GameState blocks (attackUnit units unitToAttack (power unit)) rounds)

attackUnit :: GameMap Unit -> (Coordinate, Unit) -> Int -> GameMap Unit
attackUnit units (pos, unit) power =
    let unit' = decreaseHp power unit
    in  if hp unit' <= 0 then M.delete pos units else M.insert pos unit' units

unitMove :: Coordinate -> [Coordinate] -> Unit -> State GameState Coordinate
unitMove from inRangePositions unit = do
    gs@(GameState blocks units rounds) <- get
    let distances = mapMaybe
            (\to -> (to, ) <$> snd (reachableIn gs from to))
            inRangePositions
    -- traceShowM inRangePositions
    -- traceShowM distances
    if null distances
        then return from
        else do
            let
                closest = fst $ minimumBy
                    (comparing snd <> (readingOrder `on` fst))
                    distances
            let reachables =
                    M.toList
                        . M.filterWithKey
                              (\k v -> isNeighbor k from && isJust v)
                        . fst
                        $ reachableIn gs closest from
            let unitOrder =
                    comparing (fromJust . snd) <> (readingOrder `on` fst)
            let (pos', _) = minimumBy unitOrder reachables
            let units'    = M.insert pos' unit . M.delete from $ units
            -- traceM
            --     (  "moving unit "
            --     ++ show unit
            --     ++ " from "
            --     ++ show from
            --     ++ " to "
            --     ++ show pos'
            --     ++ " (closest is "
            --     ++ show closest
            --     ++ ")"
            --     )
            put (GameState blocks units' rounds)
            return pos'

main :: IO ()
main = do
    -- part 1
    -- input <- parseGame 3 <$> getInput 15
    -- let GameState _ units rounds = execState (untilM_ playRound isOver) input
    -- printWithTime $ rounds * sum (M.map hp units)

    -- -- part 2
    forM_ [15 ..] $ \power -> do
        input <- parseGame power <$> getInput 15
        let (deaths, GameState _ units rounds) =
                runState (untilM playRound isOver) input
        unless (any (> 0) deaths) $ do
            print power
            print rounds
            print $ M.map hp units
            print $ rounds * sum (M.map hp units)
