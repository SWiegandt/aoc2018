module Day13.Day13 where

import           Util.IO
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.List
import           Data.Ord
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Loops
import           Control.Monad
import           Debug.Trace

type Coordinate = (Int, Int)
type Velocity = (Int, Int)
data PathDir = H | V
data Path = Straight PathDir | Curve Int | Intersection
data Rail = Rail { path :: Path, pos :: Coordinate } | Cart { vel :: Velocity, rail :: Rail, intersection :: Int }
type Track = M.Map Coordinate Rail
newtype TrackPrinter = TrackPrinter Track

instance Show Path where
    show (Straight H   ) = "-"
    show (Straight V   ) = "|"
    show (Curve    (-1)) = "/"
    show (Curve    1   ) = "\\"
    show Intersection    = "+"

instance Show Rail where
    show (Rail p _         ) = show p
    show (Cart (-1, 0 ) _ _) = "<"
    show (Cart (1 , 0 ) _ _) = ">"
    show (Cart (0 , -1) _ _) = "^"
    show (Cart (0 , 1 ) _ _) = "v"

instance Show TrackPrinter where
    show (TrackPrinter track) =
        let
            maxX = maximum . map fst . M.keys $ track
            maxY = maximum . map snd . M.keys $ track
        in
            concatMap
                (\y ->
                    concatMap (\x -> maybe " " show (track M.!? (x, y)))
                              [0 .. maxX]
                        ++ "\n"
                )
                [0 .. maxY]

parseCart :: Char -> Coordinate -> Maybe Rail
parseCart '<' c = Just $ Cart (-1, 0) (Rail (Straight H) c) 0
parseCart '>' c = Just $ Cart (1, 0) (Rail (Straight H) c) 0
parseCart '^' c = Just $ Cart (0, -1) (Rail (Straight V) c) 0
parseCart 'v' c = Just $ Cart (0, 1) (Rail (Straight V) c) 0
parseCart _   _ = Nothing

parseRail :: Char -> Coordinate -> Maybe Rail
parseRail '-'  c = Just $ Rail (Straight H) c
parseRail '|'  c = Just $ Rail (Straight V) c
parseRail '/'  c = Just $ Rail (Curve (-1)) c
parseRail '\\' c = Just $ Rail (Curve 1) c
parseRail '+'  c = Just $ Rail Intersection c
parseRail p    c = parseCart p c

parseTrack :: String -> Track
parseTrack =
    M.mapMaybe id
        . M.fromList
        . concatMap (\(y, s) -> zipWith (insertCoordinates y) [0 ..] s)
        . ([0 ..] `zip`)
        . lines
    where insertCoordinates y x c = ((x, y), parseRail c (x, y))

isCart :: Rail -> Bool
isCart Cart{} = True
isCart _      = False

moveCart :: Rail -> State Track (Maybe Coordinate)
moveCart r | isCart r = case path . rail $ r of
    Straight _   -> moveStraight r
    Curve    _   -> moveCurve r
    Intersection -> moveIntersection r
moveCart _ = error "Tried moving non-cart path"

moveStraight :: Rail -> State Track (Maybe Coordinate)
moveStraight (Cart v@(vx, vy) (Rail p r@(x, y)) i) = do
    t <- get
    let r'@(x', y') = (x + vx, y + vy)
    executeMove (r, Rail p r) (r', Cart v (t M.! r') i)

moveCurve :: Rail -> State Track (Maybe Coordinate)
moveCurve (Cart v@(vx, vy) (Rail p@(Curve m) r@(x, y)) i) = do
    t <- get
    let v'@(vx', vy') = (vy * m, vx * m)
    let r'@(x', y')   = (x + vx', y + vy')
    executeMove (r, Rail p r) (r', Cart v' (t M.! r') i)

moveIntersection :: Rail -> State Track (Maybe Coordinate)
moveIntersection (Cart v@(vx, vy) (Rail p r@(x, y)) i) = do
    t <- get
    let v'@(vx', vy') = case i of
            0 -> (vy, vx)
            1 -> v
            2 -> (negate vy, negate vx)
        r'@(x', y') = (x + vx', y + vy')
    executeMove (r, Rail p r) (r', Cart v' (t M.! r') ((i + 1) `mod` 3))

executeMove
    :: (Coordinate, Rail)
    -> (Coordinate, Rail)
    -> State Track (Maybe Coordinate)
executeMove (c, r) (c', r') = do
    track <- get
    if isCart $ track M.! c'
        then trace (show . TrackPrinter $ track) . return $ Just c'
        else do
            modify (M.insert c r)
            modify (M.insert c' r')
            return Nothing

moveCarts :: State Track (Maybe Coordinate)
moveCarts = do
    track   <- get
    crashes <-
        mapM moveCart
        . sortBy (comparing (snd . pos . rail) <> comparing (fst . pos . rail))
        . M.elems
        . M.filter isCart
        $ track
    return $ msum crashes

main :: IO ()
main = do
    track <- parseTrack <$> getInput 13
    printWithTime . evalState (iterateUntil isJust moveCarts) $ track
