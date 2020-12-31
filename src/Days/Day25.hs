module Days.Day25 where
import           Data.List
import           Data.Maybe
import qualified Program.RunDay as R (runDay)
import           Util.NoQuotes
import           Util.Util

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = (Int, Int)

type Output1 = Int
type Output2 = NoQuotes

parser :: String -> Input
parser = listToTuple . map read . lines

--12495346
part1 :: Input -> Output1
part1 (card, door) = iterate (\n -> (n * door) `mod` 20201227) 1 !! cardLoop
    where cardLoop = fromJust $ elemIndex card $ iterate (\n-> (n*7) `mod` 20201227) 1

part2 :: Input -> Output2
part2 = const $ NoQuotes "Yay you win!"
