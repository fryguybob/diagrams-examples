{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           BoltzmannTrees
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

treeSize :: Tree a -> Int
treeSize (Leaf _) = 0
treeSize (Branch _ t1 t2) = 1 + treeSize t1 + treeSize t2

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
    triangulation' [p,q]   (Leaf _)   = (p ~~ q, Leaf (lerp 0.5 p q))
    triangulation' pts (Branch _ l r) =
      ( mconcat
          [ head pts ~~ mid
          , last pts ~~ mid
          , diags1
          , diags2
          ]
      , Branch (centroid [head pts, last pts, mid]) l' r'
      )
      where
        (pts1, mid:pts2) = splitAt (treeSize l + 1) pts
        (diags1, l') = triangulation' (pts1++[mid]) l
        (diags2, r') = triangulation' (mid:pts2) r
    drawTree (Leaf _) = mempty
    drawTree (Branch p l r) = drawTree' p l <> drawTree' p r
    drawTree' parent (Leaf p) = drawEdge parent (p .+^ (p .-. parent))
    drawTree' parent (Branch p l r) = drawEdge parent p <> drawTree' p l <> drawTree' p r
    drawEdge p q = circle 0.1 # fc grey # lw none # moveTo q <> (p ~~ q) # lc grey

main :: IO ()
main = do
  Just t <- runGenM 20 0.1 genTree
  mainWith (triangulation t # frame 1)
