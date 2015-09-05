{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Data.Maybe                          (fromJust)
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

grid :: Int -> Diagram B
grid n = mconcat
  [ bars
  , bars # rotateBy (1/4)
  , dashingG [0.1, 0.1] 0 . uncurry (~~) . fromJust . getCorners . boundingBox $ bars
  ]
  # alignBL
  where
    bars = hsep 1 (replicate (n+1) (vrule (fromIntegral n)))
         # centerXY

treeToPath  :: Tree () -> [V2 Double]
treeToPath = init . treeToPath'

treeToPath' :: Tree () -> [V2 Double]
treeToPath' (Leaf _) = [unitY]
treeToPath' (Branch _ l r) = unitX : treeToPath' l ++ treeToPath' r

drawPath :: [V2 Double] -> Diagram B
drawPath vs = alignBL . mconcat $ zipWith translate (scanl (^+^) zero vs) (map arrowV vs)

upperRegion :: Double -> [V2 Double] -> Trail V2 Double
upperRegion l vs =
  fromOffsets (l *^ unit_X : l *^ unit_Y : vs)
  # glueTrail

lowerRegion :: Double -> [V2 Double] -> Trail V2 Double
lowerRegion l vs =
  fromOffsets (l *^ unit_Y : l *^ unit_X : vs)
  # glueTrail

n = 5

dia :: Tree () -> Diagram B
dia t = mconcat
  [ drawPath vs
  , grid n
  , upperRegion n vs # strokeT # fc yellow # alignBL
  , lowerRegion n vs # strokeT # fc skyblue # alignBL
  ]
  where
    vs = treeToPath t

main :: IO ()
main = do
  Just t <- runGenM (2*n + 1) 0 genTree
  mainWith (dia t # frame 1)
