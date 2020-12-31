module Days.Day11 where
import           Control.Monad
import           Data.Bifunctor
import           Data.Matrix    (Matrix)
import qualified Data.Matrix    as Matrix
import           Data.Maybe
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type SeatMatrix = Matrix (Maybe Bool)

type Input = SeatMatrix

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = fmap processSeat . Matrix.fromLists . lines

part1 :: Input -> Output1
part1 matrix = gameOfLife 1 1 matrix matrix

part2 :: Input -> Output2
part2 matrix = gameOfLife2 1 1 matrix matrix

processSeat :: Char -> Maybe Bool
processSeat '#' = Just True
processSeat 'L' = Just False
processSeat  _  = Nothing

gameOfLife :: Int -> Int -> SeatMatrix -> SeatMatrix -> Int
gameOfLife r c prev next
  | c > Matrix.ncols prev = if prev == next
      then length $ filter id $ catMaybes $ Matrix.toList next
      else gameOfLife 1 1 next next
  | r > Matrix.nrows prev = gameOfLife 1 (c+1) prev next
  | currChar == Just False && occupiedAround == 0 = gameOfLife (r+1) c prev $ Matrix.setElem (Just True)  (r,c) next
  | currChar == Just True  && occupiedAround >= 4 = gameOfLife (r+1) c prev $ Matrix.setElem (Just False) (r,c) next
  | otherwise = gameOfLife (r+1) c prev next
  where
    currChar = Matrix.getElem r c prev
    occupiedAround = getOccupiedAround r c prev

gameOfLife2 :: Int -> Int -> SeatMatrix -> SeatMatrix -> Int
gameOfLife2 r c prev next
  | c > Matrix.ncols prev = if prev == next
      then length $ filter id $ catMaybes $ Matrix.toList next
      else gameOfLife2 1 1 next next
  | r > Matrix.nrows prev = gameOfLife2 1 (c+1) prev next
  | currChar == Just False && occupiedAround == 0 = gameOfLife2 (r+1) c prev $ Matrix.setElem (Just True) (r,c) next
  | currChar == Just True && occupiedAround >= 5 = gameOfLife2 (r+1) c prev $ Matrix.setElem (Just False) (r,c) next
  | otherwise = gameOfLife2 (r+1) c prev next
  where
    currChar = Matrix.getElem r c prev
    occupiedAround = losOccupied r c prev

getOccupiedAround :: Int -> Int -> SeatMatrix -> Int
getOccupiedAround r c matrix = sum $ mapMaybe ((fmap fromEnum . join . flip(uncurry Matrix.safeGet) matrix) . bimap (+r) (+c))
  [(x,y) | x <- [-1..1], y <- [-1..1], x /=0 || y /= 0]

losOccupied :: Int -> Int -> SeatMatrix -> Int
losOccupied r c m = length $ filter id $ zipWith (look m) (map (bimap (+r) (+c)) coords) coords
  where
    coords = [(x,y) | x <- [-1..1], y <- [-1..1], x /=0 || y /= 0]
    look :: SeatMatrix -> (Int, Int) -> (Int, Int) -> Bool
    look m (r, c) (nr, nc)
      | r > Matrix.nrows m || r < 1 || c > Matrix.ncols m || c < 1 = False
      | otherwise = fromMaybe (look m (r+nr, c+nc) (nr, nc)) $ Matrix.getElem r c m
