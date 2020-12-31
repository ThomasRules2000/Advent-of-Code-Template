{-# LANGUAGE TupleSections #-}
module Days.Day24 where
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Util.Coord
import           Util.HexTile    (HexTile)
import qualified Util.HexTile    as HexTile

import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

data Move = East
          | SouthEast
          | SouthWest
          | West
          | NorthWest
          | NorthEast
          deriving (Eq, Show)

type Input = [HexTile]

type Output1 = Int
type Output2 = Int

-- PARSER --
parser :: String -> Input
parser = map (foldr ($) HexTile.zero . getMoves) . lines

getMoves :: String -> [HexTile -> HexTile]
getMoves [] = []
getMoves (x:xs) = case x of
  'e' -> HexTile.east:next1
  'w' -> HexTile.west:next1
  'n' -> if head xs == 'e' then HexTile.northEast:next2 else HexTile.northWest:next2
  's' -> if head xs == 'e' then HexTile.southEast:next2 else HexTile.southWest:next2
  where
    next1 = getMoves xs
    next2 = getMoves $ tail xs

-- PART 1 --
part1 :: Input -> Output1
part1 = Set.size . getInputSet

getInputSet :: [HexTile] -> Set HexTile
getInputSet = Map.keysSet . Map.filter id . Map.fromListWith (/=) . fmap (, True)

-- PART 2 --
part2 :: Input -> Output2
part2 = Set.size . (!!100) . iterate (processCycle (\n -> n == 0 || n > 2) (==2)) . getInputSet
