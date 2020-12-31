module Days.Day04 where
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Program.RunDay  as R (runDay)
import           Text.Read
import           Util.Util

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Map String String]

type Output1 = Int
type Output2 = Int

-- PARSER --
parser :: String -> Input
parser = map ((Map.fromList . map (listToTuple . splitOn ":")) . words) . splitOn "\n\n"

-- PART 1 --
part1 :: Input -> Output1
part1 = length . filter (`containsKeys` ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])

-- PART 2 --
part2 :: Input -> Output2
part2 = length . filter (fromMaybe False . validateFields)

validateFields :: Map String String -> Maybe Bool
validateFields m = do
  byr <- read <$> Map.lookup "byr" m :: Maybe Int
  iyr <- read <$> Map.lookup "iyr" m :: Maybe Int
  eyr <- read <$> Map.lookup "eyr" m :: Maybe Int
  hgt <- Map.lookup "hgt" m
  hcl <- Map.lookup "hcl" m
  ecl <- Map.lookup "ecl" m
  pid <- Map.lookup "pid" m
  return $ and [byr>=1920, byr<=2002,
                iyr>=2010, iyr<=2020,
                eyr>=2020, eyr<=2030,
                validateHeight hgt,
                validateHair hcl,
                ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
                validatePid pid]

validateHeight :: String -> Bool
validateHeight h = case unit of
  "in" -> hgt' >= 59 && hgt' <= 76
  "cm" -> hgt' >= 150 && hgt' <= 193
  _    -> False
  where
    (hgt, unit) = splitAt (length h - 2) h
    hgt' = fromMaybe 0 $ readMaybe hgt :: Int

validateHair :: String -> Bool
validateHair (hash:digits) = hash == '#' && length digits == 6 && all (`elem` (['0'..'9'] ++ ['a'..'f'])) digits

validatePid :: String -> Bool
validatePid pid = case readMaybe pid :: Maybe Int of
  Just _  -> length pid == 9
  Nothing -> False
