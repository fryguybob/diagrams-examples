{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

treeSize :: Tree a -> Int
treeSize (Leaf _) = 0
treeSize (Branch _ l r) = 1 + treeSize l + treeSize r

colors = brewerSet YlOrRd 9

triangulation :: Tree' -> Diagram B
triangulation t = mconcat
  [ triangDia
  , head ps ~~ last ps
  , drawTree labelledT
  ]
  where
    (triangDia, labelledT) = triangulation' ps t
    ps :: [P2 Double]
    ps = regPoly (treeSize t + 2) 1
    -- invariant: length pts == treeSize t + 2
    triangulation' :: [P2 Double] -> Tree' -> (Diagram B, Tree (P2 Double))
    triangulation' [p,q]   (Leaf _)   = (p ~~ q # lw none, Leaf (lerp 0.5 p q))
    triangulation' pts (Branch _ l r) =
      ( mconcat
          [ head pts ~~ mid # lw none
          , last pts ~~ mid # lw none
          , diags1
          , diags2
          ]
      , Branch (centroid [head pts, last pts, mid]) l' r'
      )
      where
        (pts1, mid:pts2) = splitAt (treeSize l + 1) pts
        (diags1, l') = triangulation' (pts1++[mid]) l
        (diags2, r') = triangulation' (mid:pts2) r
    drawTree :: Tree (P2 Double) -> Diagram B
    drawTree (Leaf _) = mempty
    drawTree (Branch p l r) = drawTree' 0 p l <> drawTree' 0 p r
    drawTree' :: Int -> P2 Double -> Tree (P2 Double) -> Diagram B
    drawTree' n parent (Leaf p) = drawEdge n parent (p .+^ (p .-. parent))
    drawTree' n parent (Branch p l r) = drawEdge n parent p <> drawTree' (n+1) p l <> drawTree' (n+1) p r
    drawEdge n p q = circle 0.1 # fc c # lw none # moveTo q <> (p ~~ q) # lc c
      where c = colors !! (n `mod` 9)

main :: IO ()
main = do
  Just t <- runGenM 200 0.1 genTree
  mainWith (triangulation t # frame 1)
