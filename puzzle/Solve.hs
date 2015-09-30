module Solve where

import Pieces

import Control.Applicative

import Data.Array
import Data.List
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Monoid
import Data.Semigroup
import Control.Lens
import Linear
import Data.Ord

type Box = V3 Int
type Point = V3 Int

type LPiece = [Point]

type Space a = Array Box a

emptySpace :: Box -> a -> Space a
emptySpace b i = listArray (zero, b ^-^ pure 1) (cycle [i])

checkPiece :: LPiece -> Space Int -> Bool
checkPiece ps s = and [(s ! p) == 0 | p <- ps]

inBounds :: Box -> LPiece -> Bool
inBounds (V3 bi bj bk) ps = and [isIn p | p <- ps]
  where
    isIn (V3 i j k)
      | i <   0 || j <   0 || k <   0 = False
      | i >= bi || j >= bj || k >= bk = False
      | otherwise                     = True

extent :: Piece -> Box
extent = extentL . locatePiece zero

extentL = fmap maximum . sequence 

allPlacements :: Piece -> Box -> [LPiece]
allPlacements p (V3 bx by bz)
    = [ locatePiece (V3 i j k) p
      | i <- [0..bx-x]
      , j <- [0..by-y]
      , k <- [0..bz-z]
      ]
  where
    (V3 x y z) = extent p

allPlacementsL :: LPiece -> Box -> [LPiece]
allPlacementsL lp b = [map (^+^ o) lp | o <- range (zero, b ^-^ pure 1 ^-^ e)]
  where
    e = extentL lp

allPlacementsAndRotations :: Piece -> Box -> [LPiece]
allPlacementsAndRotations p b = 
    [ lp 
    | p' <- rotations (locatePiece zero p)
    , lp <- allPlacementsL p' b
    ]

unless False a = a
unless True  _ = return ()

prop_AllInBounds :: Piece -> IO Bool
prop_AllInBounds p = do
    unless t $ do
      print ("low", low)
      print ("high", high)
      print ("b", b)
      print . head . filter (not . inBounds b . snd) . zip [0..] $ allPlacementsAndRotations p b
    return t
  where
    lp = locatePiece zero p

    low  = fmap minimum . sequence $ lp
    high = fmap maximum . sequence $ lp

    b = pure . succ . maximum . F.foldMap (:[]) $ (high ^-^ low)

    t = all (inBounds b) $ allPlacementsAndRotations p b

toPiece :: LPiece -> [[String]]
toPiece lp = p
  where
    low  = fmap minimum . sequence $ lp
    high = fmap maximum . sequence $ lp

    (V3 xs ys zs) = sequence $ [low, high]

    p = [[[ if (V3 x y z) `elem` lp then 'x' else ' ' | x <- xs ] | y <- ys ] | z <- zs ]

printPiece :: [[String]] -> IO ()
printPiece ls = do
    putStr . intercalate "-----\n" . map unlines $ ls
    putStrLn "."

-- About Z 90:    About Y 90:
-- 
-- [ 0 (-1) 0 ]   [   0   0  1 ]
-- [ 1   0  0 ]   [   0   1  0 ]
-- [ 0   0  1 ]   [ (-1)  0  0 ]
--
-- Each face (6) can be top.  For each face as top, we have four rotations.
--   6 * 4 = 24
--
-- Faces:
--   id
--   y
--   y y
--   y y y
--   z y
--   z y y y
rotations :: LPiece -> [LPiece]
rotations lp =
    [ alignCorner (map ((r !*! f) !*) lp)
    | f <- faces
    , r <- rotations
    ]
  where
    z = V3 (V3 0 (-1) 0)
           (V3 1   0  0)
           (V3 0   0  1)

    y = V3 (V3   0  0  1)
           (V3   0  1  0)
           (V3 (-1) 0  0)

    faces     = [ identity, y, y !*! y, y !*! y !*! y, y !*! z, y !*! y !*! y !*! z ]
    rotations = [ identity, z, z !*! z, z !*! z !*! z ]

alignCorner :: LPiece -> LPiece
alignCorner lp = fmap (\v -> v ^-^ low) lp
  where
    low  = fmap minimum . sequence $ lp

applyPiece :: Int -> LPiece -> Space Int -> Space Int
applyPiece name ps s = array (bounds s) (patch name (sort ps) (assocs s))

patch :: Int -> LPiece -> [(Point,Int)] -> [(Point,Int)]
patch name ps as = go as ps
  where
    go []     _      = []
    go as     []     = as
    go ((a@(i,_)):as) pss@(i':ps)
      | i == i'      = (i,name) : go as ps
      | otherwise    = a : go as pss

locatePiece :: Point -> Piece -> LPiece
locatePiece p (_,ls) = [ p ^+^ (V3 i j k)
                       | (i,l) <- zip is ls
                       , (j,r) <- zip is l
                       , (k,c) <- zip is r
                       , c == 'x'
                       ]
  where
    is = [0..]

solveStep :: Int -> Piece -> Box -> Space Int -> [Space Int]
solveStep name p b s = [ applyPiece name lp s 
                       | lp <- allPlacementsAndRotations p b
                       , checkPiece lp s
                       ]

area :: Piece -> Int
area = getProduct . F.foldMap Product . extentL . locatePiece (pure 1) 

solve :: [Piece] -> Box -> [Space (Maybe String)]
solve pieces b = fmap (fmap name) $ go 1 ps (emptySpace b 0)
  where
    ps = reverse $ sortBy (comparing area) pieces

    go :: Int -> [Piece] -> Space Int -> [Space Int]
    go _ [] s = [s]
    go i (p:ps) s = let ss = solveStep i p b s
                    in  concatMap (go (i+1) ps) ss

    m = M.fromList . zip [1..] . map fst $ ps

    name i = i `M.lookup` m

