{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BangPatterns              #-}
import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)

import Linear.Matrix              ((!*!))

import Frames

data Tree a = Bin (Tree a) a (Tree a) | Leaf a

full 0 = Leaf 0
full n = Bin (full (n-1)) n (full (n-1))

stickZ p = origin ~~ (0 ^& 0 ^& 1)
        <> p # translate unitZ

branch = 20
tilt   = 10 

vee l r = stickZ l # rotX (  branch  @@ deg)
       <> stickZ r # rotX ((-branch) @@ deg)

rotZ a = transform (aboutZ a)
rotX a = transform (aboutX a)

onPoint n d = withName n $ \(location -> p) -> atop (d `place` p)

tree3D (Leaf _)    = mempty
tree3D (Bin l _ r) = vee vl vr
  where
    vl = tree3D l # rotZ (90 @@ deg) # rotX (  tilt  @@ deg) # scale 0.5
    vr = tree3D r # rotZ (90 @@ deg) # rotX ((-tilt) @@ deg) # scale 0.5

m  = lookAt (V3 8.4 6 3.2) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective :: Path V3 Double -> Diagram B
withPerspective d = stroke $ deform pd (translateZ (-1) d)

tree2D t r = tree3D t # rotZ r # withPerspective

spin f = map (,6) . allRotations $ frame
  where
    !bb = boundingBox . mconcat . allRotations $ f
    allRotations f = [f (n @@ turn) | i <- [0..100], let n = fromIntegral i / 100]
    frame r = f r # withEnvelope bb # bgFrame 0.05 white

main' = mainWith $ \r -> (tree2D (full 10) (r @@ turn) # pad 1.1)

main'' = gifMain (spin (tree2D t))
  where
    t = full 10

main = mainWith (map fst $ spin (tree2D t))
  where
    t = full 10
