module Days.Day05 where
import           Data.List      (sort)
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

-- PARSER --
parser :: String -> Input
parser = map (foldl (\x y -> 2*x + fromEnum (y=='B' || y=='R')) 0) . lines

part1 :: Input -> Output1
part1 = maximum

part2 :: Input -> Output2
part2 seats = fst $ head $ filter (uncurry (/=)) $ zip [head sorted..] sorted
  where sorted = sort seats
