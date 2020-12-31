module Days.Day03 where
import           Data.Matrix    (Matrix)
import qualified Data.Matrix    as Matrix
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = Matrix Bool

type Output1 = Int
type Output2 = Int

-- PARSER --
parser :: String -> Input
parser = Matrix.fromLists . map (map (=='#')) . lines

-- PART 1 --
countTrees :: Matrix Bool -> (Int,Int) -> (Int,Int) -> Int
countTrees slope (x, y) dir@(xDir, yDir)
  | y > Matrix.nrows slope = 0
  | Matrix.getElem y x slope = 1 + next
  | otherwise = next
  where
    newX = ((x + xDir - 1) `mod` Matrix.ncols slope) + 1
    next = countTrees slope (newX, y+yDir) dir

part1 :: Input -> Output1
part1 slope = countTrees slope (1,1) (3,1)

-- PART 2 --
part2 :: Input -> Output2
part2 slope = product $ map (countTrees slope (1,1)) [(1,1), (3,1), (5,1), (7,1), (1,2)]
