module Days.Day13 where
import           Data.List.Split
import           Data.Maybe
import qualified Program.RunDay  as R (runDay)
import           Text.Read       (readMaybe)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = (String, [Maybe Int])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser inp = (s, readMaybe <$> splitOn "," b)
  where [s, b] = lines inp

part1 :: Input -> Output1
part1 (s, buses) = waitTime (read s) 0 $ catMaybes buses

part2 :: Input -> Output2
part2 (_, buses) = consecDeps 0 1 0 busTuples busTuples
  where busTuples = tail $ countNothings buses 0 0

waitTime :: Int -> Int -> [Int] -> Int
waitTime time waited buses
  | null arriving = waitTime (time + 1) (waited + 1) buses
  | otherwise = waited * head arriving
  where arriving = filter (\b -> time `mod` b == 0) buses

countNothings :: [Maybe Int] -> Int -> Int -> [(Int, Int)]
countNothings [] n c = [(n,c)]
countNothings (x:xs) n c = case x of
  Nothing -> countNothings xs n $ c+1
  Just y  -> (n,c):countNothings xs y 1

consecDeps :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> Int
consecDeps start _ _ [] _ = start
consecDeps start step time ((b,n):bs) allBuses
  | time `mod` b == 0 = consecDeps start (lcm step b) (time+n) bs allBuses
  | otherwise = consecDeps newStart step newStart allBuses allBuses
  where newStart = start + step
