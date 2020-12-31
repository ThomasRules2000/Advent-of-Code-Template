module Days.Day18 where
import           Data.Char
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [String ]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = lines

part1 :: Input -> Output1
part1 = sum . map (snd . flip calcExpr 0 . filter (/= ' '))

part2 :: Input -> Output2
part2 = sum . map (snd . parseE . filter (/= ' '))

calcExpr :: String -> Int -> (String, Int)
calcExpr "" acc = ("", acc)
calcExpr (c:cs) acc = case c of
  '(' -> let (ret, newAcc) = calcExpr cs 0 in calcExpr ret newAcc
  ')' -> (cs, acc)
  '+' -> calcExpr cont $ acc + next
  '*' -> calcExpr cont $ acc * next
  _   -> calcExpr cs $ digitToInt c
  where (cont, next) = getNextVal cs

getNextVal :: String -> (String, Int)
getNextVal (c:cs) = case c of
  '(' -> calcExpr cs 0
  _   -> (cs, digitToInt c)

parseE :: String -> (String, Int)
parseE s = do
  let (s1, v) = parseT s
  parseE' s1 v

parseT :: String -> (String, Int)
parseT s = let (s1, v) = parseV s in
            parseT' s1 v

parseV :: String -> (String, Int)
parseV ('(':s) = let (s1, v) = parseE s in (tail s1, v)
parseV (c:s)   = (s, digitToInt c)

parseE' :: String -> Int -> (String, Int)
parseE' ('*':s) n = let (s1, m) = parseT s
                    in parseE' s1 $ n*m
parseE' s n = (s,n)

parseT' :: String -> Int -> (String, Int)
parseT' ('+':s) n = let (s1, m) = parseV s
                    in parseT' s1 $ n+m
parseT' s n = (s,n)
