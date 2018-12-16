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

newtype Input = Input (M.IntMap Int) deriving Show
newtype Op = Op { def :: [Int] } deriving Show
newtype Output = Output (M.IntMap Int) deriving (Show, Eq)
data Sample = Sample { input :: Input, op :: Op, output :: Output } deriving Show
type Opcode = Op -> Input -> Output

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
    -> (Int -> Maybe Int)
    -> (Int -> Maybe Int)
    -> (Int -> Int -> Int)
    -> Input
    -> Output
runOp (Op [_, a, b, c]) f g action (Input i) =
    Output $ M.insert c (fromMaybe 0 (f a) `action` fromMaybe 0 (g b)) i

rrOp :: (Int -> Int -> Int) -> Op -> Input -> Output
rrOp f op input@(Input i) = runOp op (i M.!?) (i M.!?) f input

riOp :: (Int -> Int -> Int) -> Op -> Input -> Output
riOp f op input@(Input i) = runOp op (i M.!?) Just f input

irOp :: (Int -> Int -> Int) -> Op -> Input -> Output
irOp f op input@(Input i) = runOp op Just (i M.!?) f input

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
gtir = irOp (\a b -> if a > b then 1 else 0)
gtri = riOp (\a b -> if a > b then 1 else 0)
gtrr = rrOp (\a b -> if a > b then 1 else 0)
eqir = irOp (\a b -> if a == b then 1 else 0)
eqri = riOp (\a b -> if a == b then 1 else 0)
eqrr = rrOp (\a b -> if a == b then 1 else 0)

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
applyOp ops input op@(Op [a, _, _, _]) =
    let opCode        = ops !! a
        Output output = opCode op (Input input)
    in  output

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
    let toOpCode = head . def . op
    let opCodeGroups =
            zip [0 ..]
                . groupBy ((==) `on` toOpCode)
                . sortOn toOpCode
                $ samples

    -- print . map (uncurry $ identifyGroup ops) $ opCodeGroups -- sudokuify problem
    program <- parseProgram <$> getInput 16
    printWithTime . (M.! 0) . foldl' (applyOp ops) M.empty $ program
