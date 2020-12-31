module Days.Day21 where
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import           Util.NoQuotes
import           Util.Util

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = ([(Set String, Set String)], Map String (Set String))

type Output1 = Int
type Output2 = NoQuotes

parser :: String -> Input
parser s = (allergIngPairs, possPairs)
  where
    allergIngPairs = map (processIngredients . listToTuple . splitOn "(contains ") $ lines s
    possPairs = foldr getPossAllergIngs Map.empty allergIngPairs

part1 :: Input -> Output1
part1 (allergIngPairs, possPairs) = sum (map (Set.size . flip Set.difference (Set.unions $ map snd $ Map.toList possPairs) . fst) allergIngPairs)

part2 :: Input -> Output2
part2 (_, possPairs) = NoQuotes $ intercalate "," $ map snd $ Map.toList $ getMatching (Map.toList possPairs)

main :: IO ()
main = do
  allergIngPairs <- map (processIngredients . listToTuple . splitOn "(contains ") . lines <$> readFile "input.txt"
  let possPairs = foldr getPossAllergIngs Map.empty allergIngPairs
  print $ sum (map (Set.size . flip Set.difference (Set.unions $ map snd $ Map.toList possPairs) . fst) allergIngPairs)
  putStrLn $ intercalate "," $ map snd $ Map.toList $ getMatching (Map.toList possPairs)

processIngredients :: (String, String) -> (Set String, Set String)
processIngredients (ings, allergs) = listToTuple $ map Set.fromList [words ings, splitOn ", " $ init allergs]

getPossAllergIngs :: (Set String, Set String) -> Map String (Set String) -> Map String (Set String)
getPossAllergIngs (ings, allergs) = Map.unionWith Set.intersection (Map.fromSet (const ings) allergs)

getMatching :: [(String, Set String)] -> Map String String
getMatching [] = Map.empty
getMatching ((allerg, ings):rest) = Map.insert allerg match $ getMatching newList
  where
    match = head $ Set.toList ings
    newList = sortOn (Set.size . snd) $ map (fmap $ Set.delete match) rest
