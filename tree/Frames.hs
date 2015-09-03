{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE BangPatterns              #-}
module Frames
    (
    ) where

import Control.Monad

import Diagrams.Prelude hiding (output,width,height)
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import Diagrams.Backend.CmdLine
import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)

import Linear.Matrix              ((!*!))

import System.FilePath (addExtension, splitExtension)
import Text.Printf

type DR n = QDiagram Rasterific V2 n Any

instance TypeableFloat n =>  Mainable [DR n] where
  type MainOpts [DR n] = DiagramOpts

  mainRender opts d = frameRender opts d

chooseRender :: TypeableFloat n => DiagramOpts -> DR n -> IO ()
chooseRender opts d
  | null path = putStrLn "No output file given."
  | otherwise = renderRasterific path sz d
  where
    path = opts^.output
    sz   = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)

frameRender :: TypeableFloat n => DiagramOpts
    -> [DR n]
    -> IO ()
frameRender opts frames = do
  let nDigits = length . show . length $ frames
  forM_ (zip [1..] frames) $ \(i,d) -> chooseRender (indexize output nDigits i opts) d

-- | @indexize d n@ adds the integer index @n@ to the end of the
--   output file name, padding with zeros if necessary so that it uses
--   at least @d@ digits.
indexize :: Lens' s FilePath -> Int -> Integer -> s -> s
indexize out nDigits i opts = opts & out .~ output'
  where fmt         = "%0" ++ show nDigits ++ "d"
        output'     = addExtension (base ++ printf fmt i) ext
        (base, ext) = splitExtension (opts^.out)
