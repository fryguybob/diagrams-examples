{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BangPatterns              #-}
import Control.Monad

import Diagrams.Prelude hiding (size)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)

import Linear.Matrix              ((!*!))

import Frames

import BoltzmannTrees

full 0 = Leaf
full n = Branch (full (n-1)) (full (n-1))

stickZ p = origin ~~ (0 ^& 0 ^& 1)
        <> p # translate unitZ

branch = 20
tilt   = 10 

vee l r = stickZ l # rotX (  branch  @@ deg)
       <> stickZ r # rotX ((-branch) @@ deg)

rotZ a = transform (aboutZ a)
rotX a = transform (aboutX a)

onPoint n d = withName n $ \(location -> p) -> atop (d `place` p)

tree3D 0 _            = mempty
tree3D n Leaf         = mempty
tree3D n (Branch l r) = vee vl vr
  where
    vl = tree3D (n-1) l # rotZ (90 @@ deg) # rotX (  tilt  @@ deg) # scale 0.9
    vr = tree3D (n-1) r # rotZ (90 @@ deg) # rotX ((-tilt) @@ deg) # scale 0.9

tree3D' Leaf         = mempty
tree3D' (Branch l r) = vee vl vr
  where
    vl = tree3D' l # rotZ (90 @@ deg) # rotX (  tilt  @@ deg) # scale 0.9
    vr = tree3D' r # rotZ (90 @@ deg) # rotX ((-tilt) @@ deg) # scale 0.9

m  = lookAt (V3 8.4 6 3.2) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective :: Path V3 Double -> Diagram B
withPerspective d = stroke $ deform pd (translateZ (-1) d)

tree2D t r = tree3D 10 t # rotZ r # withPerspective

spin f = map (,6) . allRotations $ frame
  where
    !bb = boundingBox . mconcat . allRotations $ f
    allRotations f = [f (n @@ turn) | i <- [0..100], let n = fromIntegral i / 100]
    frame r = f r # withEnvelope bb # bgFrame 0.05 white

mainSingle = mainWith $ \r -> (tree2D (full 10) (r @@ turn) # pad 1.1)

mainFull = gifMain (spin (tree2D t))
  where
    t = full 10

mainRand = do
    Just t <- runGenM 1000 0.15 genTree
    putStrLn "Tree built"

    putStrLn $ "Size: " ++ show (size t)
    -- mainWith (tree2D t (0 @@ turn))
    gifMain (spin (tree2D t))

mainFrames = mainWith (map fst $ spin (tree2D t))
  where
    t = full 10

main = mainFull
