module Day16.Day16 where

import           Util.IO
import           Data.List.Split
import           Data.List
import           Data.Char
import qualified Data.IntMap.Strict            as M
import           Data.Bits
import           Data.Function
import           Data.Maybe
import           Control.Arrow

type Input = M.IntMap Int
type Op = [Int]
type Output = M.IntMap Int
type Opcode = Op -> Input -> Output
data Sample = Sample { input :: Input, op :: Op, output :: Output } deriving Show

parseSamples :: String -> [Sample]
parseSamples s =
    let sampleSection = head $ splitOn "\n\n\n\n" s
        samples       = splitOn "\n\n" sampleSection
    in  map parseSample samples

parseProgram :: String -> [Op]
parseProgram s =
    let programSection = last $ splitOn "\n\n\n\n" s
        ops            = filter (not . null) . splitOn "\n" $ programSection
    in  map parseOp ops

parseSample :: String -> Sample
parseSample s =
    let rows = splitOn "\n" s
    in  Sample (parseInOut $ head rows)
               (parseOp $ rows !! 1)
               (parseInOut $ rows !! 2)

parseInOut :: String -> M.IntMap Int
parseInOut s =
    let nums = filter isDigit s
    in  M.fromList . zip [0 ..] $ map digitToInt nums

parseOp :: String -> Op
parseOp = map read . words

runOp
    :: Op
    -> (Int -> Maybe Int)
    -> (Int -> Maybe Int)
    -> (Int -> Int -> Int)
    -> Input
    -> Output
runOp [_, a, b, c] f g action =
    M.insert c (fromMaybe 0 (f a) `action` fromMaybe 0 (g b))

rrOp :: (Int -> Int -> Int) -> Op -> Input -> Output
rrOp f op input = runOp op (input M.!?) (input M.!?) f input

riOp :: (Int -> Int -> Int) -> Op -> Input -> Output
riOp f op input = runOp op (input M.!?) Just f input

irOp :: (Int -> Int -> Int) -> Op -> Input -> Output
irOp f op input = runOp op Just (input M.!?) f input

addr = rrOp (+)
addi = riOp (+)
mulr = rrOp (*)
muli = riOp (*)
banr = rrOp (.&.)
bani = riOp (.&.)
borr = rrOp (.|.)
bori = riOp (.|.)
setr = rrOp const
seti = irOp const
gt a b = fromEnum (a > b)
gtir = irOp gt
gtri = riOp gt
gtrr = rrOp gt
eq a b = fromEnum (a == b)
eqir = irOp eq
eqri = riOp eq
eqrr = rrOp eq

agreesWithOpcode :: Sample -> Opcode -> Bool
agreesWithOpcode (Sample input op output) opCode = output == opCode op input

agreements :: [Opcode] -> Sample -> [Bool]
agreements ops sample = map (agreesWithOpcode sample) ops

identifyGroup :: [Opcode] -> Int -> [Sample] -> (Int, [Int])
identifyGroup ops opCode samples =
    ( opCode
    , map fst
        . filter (\(_, ls) -> length (filter id ls) == length samples)
        . zip [0 ..]
        . transpose
        . map (agreements ops)
        $ samples
    )

applyOp :: [Opcode] -> M.IntMap Int -> Op -> M.IntMap Int
applyOp ops input op@[a, _, _, _] = (ops !! a) op input

main :: IO ()
main = do
    samples <- parseSamples <$> getInput 16
    let ops =
            [ gtrr
            , borr
            , gtir
            , eqri
            , addr
            , seti
            , eqrr
            , gtri
            , banr
            , addi
            , setr
            , mulr
            , bori
            , muli
            , eqir
            , bani
            ]

    -- part 1
    let applied = map (agreements ops) samples
    printWithTime . length . filter ((>= 3) . length . filter id) $ applied

    -- part 2
    let toOpCode = head . op
    let opCodeGroups =
            zip [0 ..]
                . groupBy ((==) `on` toOpCode)
                . sortOn toOpCode
                $ samples

    -- print . map (uncurry $ identifyGroup ops) $ opCodeGroups -- sudokuify problem
    program <- parseProgram <$> getInput 16
    printWithTime . (M.! 0) . foldl' (applyOp ops) M.empty $ program
