module Days.Day17 where
import           Data.Maybe
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Vector    (Vector)
import qualified Data.Vector    as Vec
import qualified Program.RunDay as R (runDay)
import           Util.Coord

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Vector Char]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map Vec.fromList . lines

part1 :: Input -> Output1
part1 input = Set.size $ (!!6) $ iterate (processCycle (\n -> n /= 2 && n /= 3) (==3)) (getInitialConfig input :: Set Coord3D)

part2 :: Input -> Output2
part2 input = Set.size $ (!!6) $ iterate (processCycle (\n -> n /= 2 && n /= 3) (==3)) (getInitialConfig input :: Set Coord4D)

getInitialConfig :: (Coord c) => [Vector Char] -> Set c
getInitialConfig vs = go vs 0 []
  where
    go :: (Coord c) => [Vector Char] -> Int -> [[Maybe c]] -> Set c
    go [] _ is     = Set.fromList $ catMaybes $ concat is
    go (v:vs) n is = go vs (n+1) $ Vec.toList (Vec.imap (getIndex n) v):is
    getIndex :: (Coord c) => Int -> Int -> Char -> Maybe c
    getIndex x y c
      | c == '#' = Just $ from2D (x,y)
      | otherwise = Nothing
