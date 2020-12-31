module Days.Day02 (runDay) where
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [(Int, Int, Char, String)]

type Output1 = Int
type Output2 = Int

-- PARSER --
parser :: String -> Input
parser = map (processPasswords . words) . lines

processPasswords :: [String] -> (Int, Int, Char, String)
processPasswords [nums, letterColon, pass] = (x, y, head letterColon, pass)
  where [x,y] = read <$> splitOn "-" nums :: [Int]

-- PART 1 --
isValid :: (Int, Int, Char, String) -> Bool
isValid (gt, lt, letter, pass) = numLetter <= lt && numLetter >= gt
  where numLetter = length $ filter (==letter) pass

part1 :: Input -> Output1
part1 = length . filter isValid

-- PART 2 --
isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (one, two, letter, pass) = (pass!!(one-1) /= letter) /= (pass!!(two-1) /= letter)

part2 :: Input -> Output2
part2 = length . filter isValid2
