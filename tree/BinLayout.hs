{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           BoltzmannTrees
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

drawTree :: Tree -> Diagram B
drawTree Leaf = mempty
drawTree (Branch l r) =
  vsep 2
    [ circle 0.1 # fc black
    , children # centerX
    ]
  # (withNames ["L", "R"] $ \[lt,rt] ->
       atop (origin ~~ location lt <> origin ~~ location rt)
    )
  # localize
  where
    children = hsep 1
      [ drawTree l # named "L"
      , drawTree r # named "R"
      ]

main :: IO ()
main = do
  Just t <- runGenM 1000 0.15 genTree
  mainWith (drawTree t # frame 1)
