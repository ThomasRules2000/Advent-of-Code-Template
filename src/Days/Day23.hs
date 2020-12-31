module Days.Day23 where

import           Data.Char
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.Maybe
import qualified Program.RunDay     as R (runDay)
import           Util.NoQuotes
import           Util.Util

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = (IntMap Int, Int)

type Output1 = NoQuotes

type Output2 = Int

parser :: String -> Input
parser s = (IntMap.fromList $ zip inp $ rotate 1 inp, head inp)
    where inp = map digitToInt s

part1 :: Input -> Output1
part1 inp = NoQuotes $ tail $ take 9 $ concatMap show $ getList (fst $ iterate (doMove 9) inp !! 100) 1

getList :: IntMap Int -> Int -> [Int]
getList cupMap val = val : getList cupMap next
    where next = cupMap IntMap.! val

doMove :: Int -> (IntMap Int, Int) -> (IntMap Int, Int)
doMove maxCup (cupMap, currCup) = (newMap, newMap IntMap.! currCup)
    where
        newMap =  IntMap.insert destCup cup1 $ IntMap.insert cup3 afterDest $ IntMap.insert currCup afterCup3 cupMap
        cup1 = cupMap IntMap.! currCup
        cup2 = cupMap IntMap.! cup1
        cup3 = cupMap IntMap.! cup2
        afterDest = cupMap IntMap.! destCup
        afterCup3 = cupMap IntMap.! cup3
        nextCups = [cup1, cup2, cup3]
        destCup = fromJust $ find (not . flip elem nextCups) $ map ((+1) . (`mod` maxCup) . subtract 1 . (+currCup)) [maxCup-1,maxCup-2..maxCup-4]

part2 :: Input -> Output2
part2 (cupMap, startVal) = product $ take 3 $ getList (fst $ iterate (doMove 1_000_000) (newMap, startVal) !! 10_000_000) 1
    where
        lastCup = fst $ head $ filter ((==startVal).snd) $ IntMap.toList cupMap
        newMap = IntMap.union (IntMap.fromList $ (1_000_000, startVal):(lastCup, 10):zip [10..] [11..1_000_000]) cupMap
