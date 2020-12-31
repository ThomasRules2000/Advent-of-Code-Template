module Days.Day14 where
import           Data.Bits
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay  as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

data Instruction = Mem Int Int
                  | Mask [Maybe Bool]
                  deriving (Eq, Show)

type Input = [Instruction]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = processTuples . map (listToTuple . splitOn " = ") . lines

part1 :: Input -> Output1
part1 = runInstructions [] Map.empty

part2 :: Input -> Output2
part2 = runInstructions2 [] Map.empty

processTuples :: [(String, String)] -> [Instruction]
processTuples [] = []
processTuples ((inst, val):xs)
  | take 3 inst == "mem" = Mem (read $ init $ drop 4 inst) (read val):rest
  | otherwise = Mask (map processMaskBit val):rest
  where rest = processTuples xs

processMaskBit :: Char -> Maybe Bool
processMaskBit '0' = Just False
processMaskBit '1' = Just True
processMaskBit  _  = Nothing

runInstructions :: [Maybe Bool] -> Map Int Int -> [Instruction] -> Int
runInstructions _ m [] = sum $ map snd $ Map.toList m
runInstructions mask m (i:is) = case i of
  Mem addr val -> runInstructions mask (Map.insert addr (applyMask mask val) m) is
  Mask newMask -> runInstructions newMask m is

runInstructions2 :: [[Maybe Bool]] -> Map Int Int -> [Instruction] -> Int
runInstructions2 _ m [] = sum $ map snd $ Map.toList m
runInstructions2 masks m (i:is) = case i of
  Mem addr val -> runInstructions2 masks (Map.union (Map.fromList $ map (\mask -> (applyMask mask addr, val)) masks) m) is
  Mask newMask -> runInstructions2 (allMasks newMask [[]]) m is

applyMask :: [Maybe Bool] -> Int -> Int
applyMask mask val = go (reverse mask) val 0
  where
    go :: [Maybe Bool] -> Int -> Int -> Int
    go [] val _ = val
    go (m:ms) val bitNo = case m of
      Just False -> go ms (clearBit val bitNo) $ bitNo + 1
      Just True  -> go ms (setBit val bitNo) $ bitNo + 1
      Nothing    -> go ms val $ bitNo + 1

allMasks :: [Maybe Bool] -> [[Maybe Bool]] -> [[Maybe Bool]]
allMasks [] masks = map reverse masks
allMasks (m:ms) masks = case m of
  Nothing -> allMasks ms $ ((Just False:) <$> masks) ++ ((Just True:) <$> masks)
  Just False -> allMasks ms $ (Nothing:) <$> masks
  Just True -> allMasks ms $ (Just True:) <$> masks

listToTuple :: [a] -> (a,a)
listToTuple [x,y] = (x,y)
