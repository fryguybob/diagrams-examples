{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections             #-}
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)
import Linear.Matrix              ((!*!))

import Data.List.Split

import Pieces

boxWidth = 2

box :: Path V3 Double
box = Path [f p1 ~~ f p2 | p1 <- ps, p2 <- ps, quadrance (p1 .-. p2) == 4]
  where
    ps = getAllCorners $ fromCorners (-1) 1
    f  = fmap fromIntegral

m  = lookAt (V3 8.4 6 3.2) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective :: Path V3 Double -> Diagram B
withPerspective d = stroke $ deform pd (translateZ (-1) d)

catX = beside unitX
catY = beside unitY
catZ = beside unitZ

align3X = align unitX

ell :: Path V3 Double
ell = align3X (box `catX` box `catX` box) `catY` align3X box

rotEll :: Angle Double -> Diagram B
rotEll a = bgFrame 0.1 white $ withPerspective (transform (aboutZ a) ell)

main' = defaultMain $ withPerspective ell

main'' = gifMain [(rotEll (n @@ turn), 3) | i <- [0..200], let n = fromIntegral i / 200]

main = do
    Right ps <- parsePiecesFromFile "pieces7.txt"

    let paths = map (fmap (centerXYZ . buildPiece)) ps
        table = buildTable 3 (map (fmap withPerspective) paths) Nothing

        rotations a = map (fmap (withPerspective . transform (aboutZ a))) paths

        bb = mconcat (map (boundingBox . snd) (concat $ allRotations rotations))

        rotTable a = bgFrame 0.1 white 
                   $ buildTable 3 (rotations a) (Just bb) -- Nothing

        allRotations f = [f (n @@ turn) | i <- [0..100], let n = fromIntegral i / 100]

--    defaultMain table
    gifMain (map (,6) (allRotations rotTable))

buildChunk  :: (Functor (V b), Num (N b), Monoid b, Transformable b) =>
              (a -> b) -> Vn b -> [a] -> b
buildChunk f v as = mconcat [ f a # translate (v ^* fromIntegral i) | (a,i) <- zip as [0..] ]

buildPiece :: [[String]] -> Path V3 Double
buildPiece = buildChunk buildPlane (unitZ ^* boxWidth)

buildPlane :: [String] -> Path V3 Double
buildPlane = buildChunk buildRow (unitY ^* boxWidth)

buildRow :: String -> Path V3 Double
buildRow = buildChunk f (unitX ^* boxWidth)
  where
    f 'x' = box
    f _   = mempty

buildTable :: Int -> [(String, Diagram B)] -> Maybe (BoundingBox (V B) (N B)) -> Diagram B
buildTable cols ds e = vcat' (with & sep .~ 0.1) (map (hcat' (with & sep .~ 0.1)) rs)
  where
    f n d = d # lc (colorMap n)
    ds' = map (uncurry f) ds 
    rs = chunksOf cols (map (withEnvelope bb) ds')

    -- If we are not given an envelope, take the maximal
    -- bounding box for a cell.
    bb = maybe (mconcat (map boundingBox ds')) id e

-- TODO: move this out of buildTable to make buildTable more general.
colorMap s = case s `lookup` m of
               Just c  -> c
               Nothing -> black
  where
    m = [ ("clear", lightblue)
        , ("blue", blue)
        , ("white", silver)
        , ("black", black)
        , ("orange", orange)
        , ("red", red)
        , ("yellow", yellow)
        , ("purple", purple)
        , ("green", green)
        ]
