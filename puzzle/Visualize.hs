{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE LambdaCase                #-}
module Visualize where
import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Data.Array
import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.List
import Data.Ord

import Diagrams.Prelude hiding (size)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.TwoD.Vector
import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.Core.Trace
import Diagrams.LinearMap         (amap)

import Codec.Picture              (GifDelay)

import Linear.Matrix              ((!*!))

import Pieces
import Solve

rotZ a = transform (aboutZ a)
rotX a = transform (aboutX a)

viewVector = -(V3 8.4 6 3.2)
m  = lookAt (-viewVector) zero unitZ
pm = perspective (pi/3) 0.8 (-10) 10 !*! m

pd = m44Deformation pm

withPerspective :: Path V3 Double -> Path V2 Double
withPerspective d = deform pd (translateZ (-1) d)

averageV a b = b .+^ ((a .-. b) / 2)

-- Remove any face that is not counter-clockwise under projection.
cullFaces :: (Ord n, Floating n) =>
     [Located (Trail V2 n)] -> [Located (Trail V2 n)]
cullFaces ts = map snd . filter fst . markFaces $ ts

markFaces :: (Ord n, Floating n) =>
     [Located (Trail V2 n)] -> [(Bool,Located (Trail V2 n))]
markFaces ts = map (\x -> (isCCW . unLoc $ x, x)) ts
  where
    isCCW t = withTrail (const True) (const (isLoopCCW t)) t
    isLoopCCW t = case getCorners . boundingBox $ t of
      Nothing -> True
      Just (a,b) -> 
        let o = averageV a b
        in  case explodeTrail (t `at` origin) of
              []    -> True
              (s:_) -> let u = loc s .-. o
                           v = trailOffset (unLoc s) ^+^ u
                       in  cross2 u v > 0
-- sortZ :: [Located (Trail V3 n)] -> [Located (Trail V3 n)]
sortZ = sortBy s
  where
    s = comparing (fmap (dot v . (.-. p)) . boxCenter . boundingBox)
    p = origin .+^ (-viewVector)
    v = signorm viewVector

testFace rev = toPath . (if rev then reverseTrail else id) . glueTrail . fromOffsets $ [unitX,unitY,unit_X,unit_Y]

-- spinAndProject :: Path V3 Double -> Double -> Diagram B
-- spinAndProject t = \r -> lineJoin LineJoinRound . stroke . withPerspective . rotZ r $ t
-- Transparent
-- spinAndProject t = \r -> lineJoin LineJoinRound . mconcat . map snd . filter fst 
--                       . colors _2 . map (fmap stroke) . map (_1 .~ True) . markFaces . pathTrails 
--                       . withPerspective . toPath . sortZ . pathTrails . rotZ r $ t

spinAndProject t = \r -> lineJoin LineJoinRound . mconcat . map snd . filter fst 
                       . colors _2 . map (fmap stroke) . markFaces . pathTrails 
                       . withPerspective . toPath . sortZ . pathTrails . rotZ r 
                       $ centerXYZ $ mconcat $ map snd t


-- Rubic'sish   colors l ts = zipWith (\c t -> t & l %~ (fc c)) (cycle [red,green,blue,yellow,orange]) ts
-- Color by distance
-- Transparent: colors l ts = zipWith (\c t -> t & l %~ (lw none . opacity 0.5 . fc c)) cs ts
-- Solid:       colors l ts = zipWith (\c t -> t & l %~ fc c) cs ts
colors l ts = zipWith (\c t -> t & l %~ fc c) cs ts
  where
    n = length ts
    cs = [blend (fromIntegral i / fromIntegral n) green white | i <- [0..n-1]]

frameCount = 100 :: Int
delay = 6 :: GifDelay

spin f = map (,delay) . allRotations $ frame
  where
    !bb = boundingBox . mconcat . allRotations $ f
    allRotations f = [f (n @@ turn) | i <- [0..frameCount]
                                    , let n = fromIntegral i / fromIntegral frameCount]
    frame r = f r # withEnvelope bb # bgFrame 0.05 skyblue

mainWithSpace :: Space (Maybe (Colour Double)) -> IO ()
mainWithSpace s = do
    -- let d = spinAndProject (build3D s)
    -- gifMain (spin d)
    
    let ds = [ [ square 1 # fc c # translate (fmap fromIntegral (V2 x y)) 
               | x <- [0..3], y <- [0..3], Just c <- [s ! V3 x y z]
               ] # mconcat
             | z <- [0..3]
             ]

    defaultMain (hsep 0.2 ds # pad 1.1)


mainWithSolveFromFile :: FilePath -> IO ()
mainWithSolveFromFile f = do
    parsePiecesFromFile f >>= \case
        Left e   -> print e
        Right ps -> mainWithSolve ps (V3 4 4 4)

mainWithSolve :: [Piece] -> V3 Int -> IO ()
mainWithSolve ps b =
    case solve ps b of 
      []    -> putStrLn "There is no solution :("
      (s:_) -> do
        print s
        mainWithSpace (fmap (fmap colorMap) s)

main = do
    Right ps <- parsePiecesFromFile "pieces.txt"
--    mainWithSolve (take 6 ps) (V3 4 4 4)
--    mainWithSolve (take 2 ps) (V3 4 4 4)
    mainWithSolve ps (pure 4)
-------------------------------------

box :: Path V3 Double
box = toPath $ map (\i -> face # rotZ (fromIntegral i * 360 / 4 @@ deg)) [0..3]
                 <> [ face # rotX (  90  @@ deg)
                    , face # rotX ((-90) @@ deg)
                    ]

face :: Located (Trail V3 Double)
face = fromOffsets [unitZ,unitX,unit_Z,unit_X]
     # translate (-0.5) # mapLoc (reverseTrail . glueTrail)
                              --  ^^^ this seems wrong to me!?!

build3D :: Space (Maybe (Colour Double)) -> [(Colour Double, Path V3 Double)]
build3D s = [ (c, box # translate v)
            | i <- is
            , Just c <- [s ! i]
            , let v = fmap fromIntegral i
            ]
  where
    is = range . bounds $ s


colorMap :: (Floating a, Ord a) => String -> Colour a
colorMap s = case s `M.lookup` m of
               Just c  -> c
               Nothing -> black
  where
    m = M.fromList $
        [ ("clear", lightblue)
        , ("blue", blue)
        , ("white", silver)
        , ("black", black)
        , ("orange", orange)
        , ("red", red)
        , ("yellow", yellow)
        , ("purple", purple)
        , ("green", green)
        ]
