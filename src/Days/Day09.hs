module Days.Day09 where
import           Data.Sequence  (Seq (..), (<|), (|>))
import qualified Data.Sequence  as Seq
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = Seq Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Seq.fromList . map read . lines

part1 :: Input -> Output1
part1 = getInvalid

getInvalid :: Seq Int -> Int
getInvalid = uncurry findInvalid . Seq.splitAt 25

part2 :: Input -> Output2
part2 inp = findWeakness (getInvalid inp) inp

findInvalid :: Seq Int -> Seq Int -> Int
findInvalid ss@(_:<|rest) (x:<|xs)
  | isValid x ss = findInvalid (rest|>x) xs
  | otherwise = x
  where
    isValid :: Int -> Seq Int -> Bool
    isValid n ss = elem n $ do { x <- ss; y <- ss; return (x+y) }

findWeakness :: Int -> Seq Int -> Int
findWeakness n xs@(_:<|ts) = case Seq.elemIndexL n $ Seq.scanl (+) 0 xs of
  Nothing -> findWeakness n ts
  Just i -> maximum range + minimum range
    where range = Seq.take i xs
