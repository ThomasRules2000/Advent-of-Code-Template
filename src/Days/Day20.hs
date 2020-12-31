module Days.Day20 where
import           Data.Bifunctor
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List.Split
import           Data.Matrix        (Matrix)
import qualified Data.Matrix        as Matrix
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Vector        (Vector)
import qualified Data.Vector        as Vec
import qualified Program.RunDay     as R (runDay)
import           Util.Tile          (Tile (Tile))
import qualified Util.Tile          as Tile

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = (Vector (Tile, Vector Int), Matrix Bool)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser s = (matches, habitat)
  where
    tiles = Vec.fromList $ map processTile $ splitOn "\n\n" s
    matches = countMatches tiles (Tile.getSidePerms <$> tiles)
    tileMatrix = constructMap
                $ IntMap.fromList
                $ Vec.toList
                $ Vec.map (toMap . fmap (Vec.map fst . Vec.filter ((>0) . snd) . Vec.zip (Vec.map (Tile.tileNum . fst) matches))) matches
    habitat = foldr1 (Matrix.<|>)
            $ map (foldr1 (Matrix.<->) . map Tile.getInside)
            $ Matrix.toLists
            $ Matrix.mapPos (alignTile tileMatrix) tileMatrix

part1 :: Input -> Output1
part1 (matches, _) = Vec.product
                    $ Vec.map fst
                    $ Vec.filter ((==2).snd)
                    $ Vec.map (first Tile.tileNum . fmap (Vec.length . Vec.filter (>0))) matches

part2 :: Input -> Output2
part2 (_, habitat) = length (filter id $ Matrix.toList habitat)
                    - 15 * length (filter id $ concatMap (\m -> Matrix.toList $ Matrix.mapPos (isMonster m) m) $ Tile.getDihedralGroup habitat)

processTile :: String -> Tile
processTile s = Tile (read $ init $ drop 5 num) (Matrix.fromLists $ map (=='#') <$> tile)
  where (num:tile) = lines s

countMatches :: Vector Tile -> Vector (Set (Vector Bool)) -> Vector (Tile, Vector Int)
countMatches ts tileList = Vec.imap (getMatches tileList) ts
  where
    getMatches :: Vector (Set (Vector Bool)) -> Int -> Tile -> (Tile, Vector Int)
    getMatches tileList i t  = (t, Vec.imap (checkMatch t i) tileList)
    checkMatch :: Tile -> Int -> Int -> Set (Vector Bool) -> Int
    checkMatch t i j set
      | i == j = 0
      | otherwise = Set.size $ Set.intersection (Tile.getSidePerms t) set

toMap :: (Tile, Vector Int) -> (Int, (Tile, Vector Int))
toMap (t, vec) = (Tile.tileNum t, (t, vec))

constructMap :: IntMap (Tile, Vector Int) -> Matrix Tile
constructMap tileMap = snd $ foldl (getTile tileMap)
                                    (Set.union corners edges, foldr (uncurry Matrix.setElem) (Matrix.fromList 12 12 (replicate 144 Tile.empty)) outside)
                                    (circleIn 11 2)
  where
    css@(c:cs) = IntMap.keys $ IntMap.filter ((==2) . Vec.length . snd) tileMap
    corners = Set.fromList css
    edges = Set.fromList $ IntMap.keys $ IntMap.filter ((==3) . Vec.length . snd) tileMap
    outside = zip (getOutside tileMap c (Set.fromList cs) edges) $ getOutsideIndexes 12 12

getOutside :: IntMap (Tile, Vector Int) -> Int -> Set Int -> Set Int -> [Tile]
getOutside tileMap t corners edges
  | not $ null adjCorners = fst tile : getOutside tileMap adjCorner (Set.delete adjCorner corners) edges
  | not $ null adjEdges = fst tile : getOutside tileMap adjEdge corners (Set.delete adjEdge edges)
  | otherwise = [fst tile]
  where
    tile = tileMap IntMap.! t
    adjacent = Set.fromList $ Vec.toList $ snd tile
    adjCorners = Set.toList $ Set.intersection adjacent corners
    adjCorner = head adjCorners
    adjEdges = Set.toList $ Set.intersection adjacent edges
    adjEdge = head adjEdges

getTile :: IntMap (Tile, Vector Int) -> (Set Int, Matrix Tile) -> ((Int,Int), (Int, Int)) -> (Set Int, Matrix Tile)
getTile tileMap (used, tileMatrix) (coords, lookCoords) = (Set.insert tileNum used, Matrix.setElem tile coords tileMatrix)
  where
    tileNum = head (Set.toList $ Set.difference (Set.fromList $ Vec.toList $ snd $ tileMap IntMap.! Tile.tileNum (tileMatrix Matrix.! lookCoords)) used)
    tile = fst $ tileMap IntMap.! tileNum

getOutsideIndexes :: Int -> Int -> [(Int,Int)]
getOutsideIndexes maxX maxY =  [(x,1) | x <- [1..maxX]]
                            ++ [(maxX, y) | y <- [2..maxY]]
                            ++ [(x,maxY) | x <- [maxX-1,maxX-2..1]]
                            ++ [(1,y) | y <- [maxY-1,maxY-2..2]]

circleIn :: Int -> Int -> [((Int,Int), (Int,Int))]
circleIn 0 _ = []
circleIn ringSize offset = doRing offset offset ringSize ringSize ++ circleIn (ringSize - 1) (offset+1)
  where doRing :: Int -> Int -> Int -> Int -> [((Int,Int), (Int,Int))]
        doRing minX minY maxX maxY = [((x, minY), (x, minY-1)) | x <- [minX..maxX]]
                                  ++ [((maxX, y), (maxX+1, y)) | y <- [minY+1..maxY]]
                                  ++ [((x, maxY), (x, maxY+1)) | x <- [maxX-1,maxX-2..minX]]
                                  ++ [((minX, y), (minX-1, y))| y <- [maxY-1,maxY-2..minY+1]]

alignTile :: Matrix Tile -> (Int,Int) -> Tile -> Tile
alignTile tileMatrix (x,y) tile = head [x | x <- Tile.getDihedralTile tile, matches (Tile.getTop x) above,
                                                                            matches (Tile.getBottom x) below,
                                                                            matches (Tile.getLeft x) left,
                                                                            matches (Tile.getRight x) right]
  where above = Tile.getSidePerms <$> uncurry Matrix.safeGet (x, y-1) tileMatrix
        below = Tile.getSidePerms <$> uncurry Matrix.safeGet (x, y+1) tileMatrix
        left  = Tile.getSidePerms <$> uncurry Matrix.safeGet (x-1, y) tileMatrix
        right = Tile.getSidePerms <$> uncurry Matrix.safeGet (x+1, y) tileMatrix
        matches :: Vector Bool -> Maybe (Set (Vector Bool)) -> Bool
        matches _ Nothing         = True
        matches this (Just other) = this `Set.member` other

--                   #
-- #    ##    ##    ###
--  #  #  #  #  #  #

isMonster :: Matrix Bool -> (Int, Int) -> Bool -> Bool
isMonster habitat (x,y) _
  | x > Matrix.nrows habitat - 3 || y > Matrix.ncols habitat - 19 = False
  | otherwise = all (habitat Matrix.!) [                                                                      (x,y+18),
    (x+1, y),              (x+1,y+5), (x+1,y+6),             (x+1,y+11), (x+1,y+12),             (x+1,y+17), (x+1,y+18), (x+1,y+19),
        (x+2,y+1),     (x+2,y+4),         (x+2,y+7),     (x+2,y+10),         (x+2,y+13),     (x+2,y+16)]
