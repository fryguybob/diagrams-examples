{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.TwoD.Image
import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine

lions :: Diagram Postscript R2
lions = (rotateBy  (1/5) $ a)
    ||| (rotateBy (-1/5) $ b)
  where
    a = (square 1 # stroke) <> (image "lion.eps" 1 1)
    b = a # scale 2

main = defaultMain lions
