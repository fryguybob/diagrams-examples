{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BangPatterns              #-}
import Control.Monad
import Control.Monad.Random

import Data.Colour.Palette.BrewerSet

import Diagrams.Prelude hiding (size)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)

import Linear.Matrix              ((!*!))

import Frames

import BoltzmannTrees

import qualified Debug.Trace as T

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

tree3D Leaf         = mempty
tree3D (Branch l r) = vee vl vr
  where
    vl = tree3D l # rotZ (90 @@ deg) # rotX (  tilt  @@ deg) # scale 0.9
    vr = tree3D r # rotZ (90 @@ deg) # rotX ((-tilt) @@ deg) # scale 0.9

veeWibble l r = do
    lf <- (+0.8) . (*0.4) <$> getRandom
    rf <- (+0.8) . (*0.4) <$> getRandom
    return $ stickZ l # scale lf # rotX (  branch  @@ deg)
          <> stickZ r # scale rf # rotX ((-branch) @@ deg)

tree3DWibble Leaf = return mempty
tree3DWibble (Branch l r) = do
    vl <- scale 0.9 . rotX (  tilt  @@ deg) . rotZ (90 @@ deg) <$> tree3DWibble l
    vr <- scale 0.9 . rotX ((-tilt) @@ deg) . rotZ (90 @@ deg) <$> tree3DWibble r

    veeWibble vl vr

m  = lookAt (V3 8.4 6 3.2) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

-- withPerspective :: Path V3 Double -> Diagram B
-- withPerspective d = stroke $ deform pd (translateZ (-1) d)

color n = let cs = brewerSet RdYlGn 9 in cs !! (n `mod` 9)

traceShowId a = T.trace (show a) a

treeColors t = go t 0
  where
    depth Leaf = 0
    depth (Branch l r) = 1 + max (depth l) (depth r)

    m = depth t
    cs = brewerSet RdYlGn 9

    color n = cs !! (floor (fromIntegral (n * 18) / fromIntegral m) `min` 8)

    go Leaf _ = []
    go (Branch l r) n = (color n : go l (n+1)) ++ (color n : go r (n+1))

withPerspective :: [Kolor] -> Path V3 Double -> Diagram B
withPerspective cs d = mconcat . zipWith (\c -> lc c . strokeLocT) cs . concat
                     . explodePath $ deform pd (translateZ (-1) d)

tree2D t r = tree3D t # rotZ r # withPerspective (treeColors t)

tree2DWibble t = do
    w <- tree3DWibble t 
    return $ \r -> w # rotZ r # withPerspective (treeColors t)

spin f = map (,6) . allRotations $ frame
  where
    !bb = boundingBox . mconcat . allRotations $ f
    allRotations f = [f (n @@ turn) | i <- [0..100], let n = fromIntegral i / 100]
    frame r = f r # withEnvelope bb # bgFrame 0.05 skyblue

sky bb = mkLinearGradient (mkStops [(darkgreen,0,1), (white,0.1,1), (skyblue,1,1)])
                          a b GradPad
    where
      (a,b) = maybe (0 ^& 0, 1 ^& 1) id (getCorners bb)

mainSingle = mainWith $ \r -> (tree2D (full 10) (r @@ turn) # pad 1.1)

mainFull = gifMain (spin (tree2D t))
  where
    t = full 10

mainFullWibble = do
    w <- tree2DWibble t
    gifMain (spin w)
  where
    t = full 8

mainRand = do
    Just t <- runGenM 1000 0.15 genTree
    putStrLn "Tree built"

    putStrLn $ "Size: " ++ show (size t)
    -- mainWith (tree2D t (0 @@ turn))
    gifMain (spin (tree2D t))

mainRandWibble = do
    Just t <- runGenM 1000 0.15 genTree
    putStrLn "Tree built"

    putStrLn $ "Size: " ++ show (size t)
    w <- tree2DWibble t
    gifMain (spin w)
    -- mainWith (w (0 @@ turn))

mainFrames = mainWith (map fst $ spin (tree2D t))
  where
    t = full 10

main = mainRandWibble
