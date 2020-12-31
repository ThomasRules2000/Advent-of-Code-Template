module Days.Day08 where
import           Data.IntSet    (IntSet)
import qualified Data.IntSet    as IntSet
import           Data.Vector    (Vector)
import qualified Data.Vector    as Vec
import qualified Program.RunDay as R (runDay)
import           Util.Util

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = Vector Instruction

type Output1 = Int
type Output2 = Int

data Instruction = Jmp Int
                  | Acc Int
                  | Nop Int
                  deriving (Eq, Show)

parser :: String -> Input
parser = Vec.fromList . map (getInstruction . listToTuple . words) . lines

part1 :: Input -> Output1
part1 = fst . runProg 0 0 IntSet.empty

part2 :: Input -> Output2
part2 = findCorrect 0

getInstruction :: (String, String) -> Instruction
getInstruction (opcode, whole@(sign:num)) = case opcode of
  "jmp" -> Jmp n
  "acc" -> Acc n
  "nop" -> Nop n
  where n = case sign of
            '+' -> read num
            '-' -> read whole

runProg :: Int -> Int -> IntSet -> Vector Instruction -> (Int, Bool)
runProg n acc set prog
  | n >= length prog = (acc, True)
  | IntSet.member n set = (acc, False)
  | otherwise = case prog Vec.! n of
      Jmp operand -> runProg (n+operand) acc newSet prog
      Acc operand -> runProg (n+1) (acc+operand) newSet prog
      Nop operand -> runProg (n+1) acc newSet prog
  where newSet = IntSet.insert n set

findCorrect :: Int -> Vector Instruction -> Int
findCorrect n prog = case prog Vec.! n of
  Acc _ -> nextIndex
  Jmp operand -> if term then acc else nextIndex
    where (acc, term) = runProg 0 0 IntSet.empty $ prog Vec.// [(n, Nop operand)]
  Nop operand -> if term then acc else nextIndex
    where (acc, term) = runProg 0 0 IntSet.empty $ prog Vec.// [(n, Jmp operand)]
  where nextIndex = findCorrect (n+1) prog
