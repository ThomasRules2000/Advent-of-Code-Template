module Days.Day10 where
import           Data.List       (sort)
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence   as Seq
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = (Seq Int, Int, Seq Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser inp = (adapters, devJolts, differences)
  where
    adapters = Seq.fromList $ sort $ map read $ lines inp :: Seq Int
    (_:>devJolts) = (+3) <$> Seq.viewr adapters
    differences = getDifferences devJolts adapters

part1 :: Input -> Output1
part1 (_, _, differences) = Seq.length (Seq.filter (==1) differences) * Seq.length (Seq.filter (==3) differences)

part2 :: Input -> Output2
part2 (adapters, devJolts, _) = dynamicProgramming (Map.singleton devJolts 1) $ 0 <| adapters

getDifferences :: Int -> Seq Int -> Seq Int
getDifferences devJolts ss = Seq.zipWith (-) (ss |> devJolts) (0 <| ss)

dynamicProgramming :: Map Int Int -> Seq Int -> Int
dynamicProgramming m Empty = m Map.! 0
dynamicProgramming m (xs:|>x) = dynamicProgramming (Map.insert x (sum $ map (flip (Map.findWithDefault 0) m . (x+)) [1..3]) m) xs
