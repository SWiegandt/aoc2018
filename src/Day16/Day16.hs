module Day16.Day16 where

import           Util.IO
import           Data.List.Split
import           Data.Char
import qualified Data.IntMap.Strict            as M
import           Data.Bits

newtype Input = Input (M.IntMap Int) deriving Show
newtype Op = Op [Int] deriving Show
newtype Output = Output (M.IntMap Int) deriving (Show, Eq)
data Sample = Sample Input Op Output deriving Show
type Opcode = Op -> Input -> Output

parseSamples :: String -> [Sample]
parseSamples s =
    let sampleSection = head $ splitOn "\n\n\n\n" s
        samples       = splitOn "\n\n" sampleSection
    in  map parseSample samples

parseSample :: String -> Sample
parseSample s =
    let rows = splitOn "\n" s
    in  Sample (parseInOut Input $ head rows)
               (parseOp $ rows !! 1)
               (parseInOut Output $ rows !! 2)

parseInOut :: (M.IntMap Int -> a) -> String -> a
parseInOut cons s =
    let nums = filter isDigit s
    in  cons (M.fromList . zip [0 ..] $ map digitToInt nums)

parseOp :: String -> Op
parseOp = Op . map read . words

runOp
    :: Op
    -> (Int -> Int)
    -> (Int -> Int)
    -> (Int -> Int -> Int)
    -> Input
    -> Output
runOp (Op [_, a, b, c]) f g op (Input i) = Output $ M.insert c (f a `op` g b) i

rrOp :: (Int -> Int -> Int) -> Op -> Input -> Output
rrOp f op input@(Input i) = runOp op (i M.!) (i M.!) f input

riOp :: (Int -> Int -> Int) -> Op -> Input -> Output
riOp f op input@(Input i) = runOp op (i M.!) (`const` undefined) f input

irOp :: (Int -> Int -> Int) -> Op -> Input -> Output
irOp f op input@(Input i) = runOp op (`const` undefined) (i M.!) f input

addr = rrOp (+)
addi = riOp (+)
mulr = rrOp (*)
muli = riOp (*)
banr = rrOp (.&.)
bani = riOp (.&.)
borr = rrOp (.|.)
bori = riOp (.|.)
setr = rrOp const
seti op@(Op [_, a, b, c]) = runOp op (const a) undefined const
gtir = irOp (\a b -> if a > b then 1 else 0)
gtri = riOp (\a b -> if a > b then 1 else 0)
gtrr = rrOp (\a b -> if a > b then 1 else 0)
eqir = irOp (\a b -> if a == b then 1 else 0)
eqri = riOp (\a b -> if a == b then 1 else 0)
eqrr = rrOp (\a b -> if a == b then 1 else 0)

main :: IO ()
main = do
    samples <- parseSamples <$> getInput 16
    let ops =
            [ addr
            , addi
            , mulr
            , muli
            , banr
            , bani
            , borr
            , bori
            , setr
            , seti
            , gtir
            , gtri
            , gtrr
            , eqir
            , eqri
            , eqrr
            ]

    let applyOps (Sample input op output) =
            map (\f -> f op input == output) ops
    let applied = map applyOps samples
    print . length . filter ((>= 3) . length . filter id) $ applied
