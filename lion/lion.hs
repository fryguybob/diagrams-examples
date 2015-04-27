{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Data.Colour.SRGB (sRGB24)
import Control.Arrow (first)
import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine

-- Some helpers

-- TODO: this should be at a level that joins two halves of a path
mirror :: Diagram Postscript R2 -> Diagram Postscript R2
mirror d = d <> scaleX (-1) d

-- radial takes a list of `n` chunks that describe some
-- path in the unit square.  Each chunk is then mapped
-- to the radial wedge of the unit circle 2*pi/n.
--
-- TODO: It would be nice to be able to apply a function across the flattened
-- list.  Specifically in the case of the mane buldging y in the middle giving
-- a little gravity to the hair (larger chunks on top and rolling to smaller
-- at the bottom).
radial cs = fromVertices . map f . concat . zipWith offset [0..n-1] $ cs
  where
    offset s = map (first (+s))
    f (x,r) = let t = tau * x / n in p2 (r*cos t, r*sin t)
    n = genericLength cs

-- The "mane" part.

mane, ears, face, eyes, jaws, nose :: Diagram Postscript R2

mane = radial (replicate 9 [(0.2,1),(0.4,0.8),(0.6,0.8),(0.8,1)]) 
     # rotateBy (-1/4) # scale 400

ear = circle 50 # scaleX 0.7 # rotateBy (-0.12) 

ears = (ear # scale 0.7 <> ear)
     # moveTo (p2 (150,130)) # mirror

-- TODO: Ideally here I would want to describe this shape in terms of a
-- blending between a square and a ellipse.  
face = fromSegments [ bezier3 a b c, bezier3 d e f ]
     # mirror # translate (r2 (0,200)) # rotateBy (1/2)
  where
     [a,b,c,d,e,f] = map r2 [ (40,  0),(200,-140),( 190,-200)
                            , ( 0,-60),(-80,-200),(-190,-200)
                            ]

eyes = circle 10 # moveTo (p2 (50,75))   # mirror # fc black

jaws = fromSegments [ bezier3 a b c ]
     # mirror # scaleY 0.8 # translate (r2 (0,-42))
  where
     [a,b,c] = map r2 [(0,-50),(0,-160),(100,-70)]

nose = circle 70 # scaleY 0.6

lion :: Diagram Postscript R2
lion = mconcat . reverse $
     [ mane # fc chocolate
     , ears # fc yellow
     , face # fc orange
     , eyes
     , jaws
     , nose # fc darkchocolate
     ]
  where darkchocolate = sRGB24 105 53 15

main = defaultMain lion
