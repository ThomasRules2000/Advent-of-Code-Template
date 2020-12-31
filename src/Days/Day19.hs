module Days.Day19 where
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (nub)
import           Data.List.Split
import           Data.Maybe         (fromJust)
import qualified Program.RunDay     as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type RuleMap = IntMap (Either Char [[Int]])

type Input = (RuleMap, [String])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser s = (rules, msgs)
  where
      [rs, msgs] = map lines $ splitOn "\n\n" s
      rules = IntMap.fromList $ map processRule rs

part1 :: Input -> Output1
part1 (rules, msgs) = length $ filter (doesParse rules) msgs

part2 :: Input -> Output2
part2 (rules, msgs) = length $ filter (doesParse (IntMap.union (IntMap.fromList $ map processRule ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]) rules)) msgs

processRule :: String -> (Int, Either Char [[Int]])
processRule s = (read l, getRHS r)
  where
    [l, r] = splitOn ": " s
    getRHS :: String -> Either Char [[Int]]
    getRHS rhs@(r:rs)
      | r == '\"' = Left  $ head rs
      | otherwise = Right $ map read . words <$> splitOn " | " rhs

doesParse :: RuleMap -> String -> Bool
doesParse rules word = "" `elem` parseWithRule rules [word] 0

parseWithRule :: RuleMap -> [String] -> Int -> [String]
parseWithRule rMap ss rule = nub $ filter (not . null) ss >>= doParse rMap rule
  where
    doParse :: RuleMap -> Int -> String -> [String]
    doParse rMap rule str@(s:ss) = case fromJust $ IntMap.lookup rule rMap of
      Left c    -> [ss | s == c]
      Right rss -> rss >>= foldl (parseWithRule rMap) [str]
