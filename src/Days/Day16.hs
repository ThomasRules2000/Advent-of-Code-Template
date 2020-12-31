{-# LANGUAGE TupleSections #-}
module Days.Day16 where
import           Data.IntSet     (IntSet)
import qualified Data.IntSet     as IntSet
import           Data.Ix         (range)
import           Data.List       (isPrefixOf, sortOn)
import           Data.List.Split
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vec
import qualified Program.RunDay  as R (runDay)
import           Util.Field      (Field (..))
import qualified Util.Field      as Field

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = ([Field], Vector Int, [[Int]], IntSet)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser s = (fields, myTicket, nearby, validVals)
  where
    [f,m,n] = map lines $ splitOn "\n\n" s
    fields = map (processField . listToTuple . splitOn ": ") f
    myTicket = Vec.fromList $ processTicket $ last m
    nearby = map processTicket $ tail n
    validVals = IntSet.unions $ map fieldSet fields

part1 :: Input -> Output1
part1 (_, _, nearby, validVals) = sumInvalid (concat nearby) validVals

part2 :: Input -> Output2
part2 (fields, myTicket, nearby, validVals) = Vec.product
                                            $ Vec.map snd
                                            $ Vec.filter (isPrefixOf "departure" . fst)
                                            $ flip Vec.zip myTicket
                                            $ Vec.update (Vec.replicate (length myTicket) "")
                                            $ Vec.fromList
                                            $ fieldToIndex IntSet.empty
                                            $ sortOn (IntSet.size . snd)
                                            $ getValidFieldNumbers (map (, IntSet.fromList [0..(length myTicket - 1)]) fields)
                                            $ map Vec.fromList
                                            $ filter (all (`IntSet.member` validVals)) nearby

processField :: (String, String) -> Field
processField (name, vals) = Field name $ IntSet.unions
                                        $ map (IntSet.fromList . range . listToTuple . map read . splitOn "-")
                                        $ splitOn " or " vals

processTicket :: String -> [Int]
processTicket = map read . splitOn ","

getValidFieldNumbers :: [(Field, IntSet)] -> [Vector Int] -> [(Field, IntSet)]
getValidFieldNumbers fields [] = fields
getValidFieldNumbers fields (v:vs) = getValidFieldNumbers (map (checkField $ Vec.toList $ Vec.indexed v) fields) vs
  where
    checkField :: [(Int, Int)] -> (Field, IntSet) -> (Field, IntSet)
    checkField [] f = f
    checkField ((i,val):vals) (f, is)
      | val `IntSet.member` fieldSet f = checkField vals (f, is)
      | otherwise = checkField vals (f, IntSet.delete i is)

fieldToIndex :: IntSet -> [(Field, IntSet)] -> [(Int, String)]
fieldToIndex _ [] = []
fieldToIndex used ((f,s):fs) = (num, fieldName f):fieldToIndex (IntSet.insert num used) fs
  where num = head $ IntSet.toList $ s IntSet.\\ used

sumInvalid :: [Int] -> IntSet -> Int
sumInvalid [] valid = 0
sumInvalid (x:xs) valid
  | IntSet.member x valid = next
  | otherwise = x + next
  where next = sumInvalid xs valid

listToTuple :: [a] -> (a,a)
listToTuple [x,y] = (x,y)
