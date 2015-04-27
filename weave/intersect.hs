import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import Diagrams.TwoD.Segment

import Data.List
import Data.Function
import Data.Ord

import Data.Foldable

d :: Diagram B
d =   strokeP (foldMap toPath as) # lc blue
    <> strokeP (foldMap toPath bs) # lc red
  where
    a = mkFixedSeg $ bézier3 a1 a2 a3 `at` (0 ^& 0)
    b = mkFixedSeg $ bézier3 b1 b2 b3 `at` (0 ^& 2)

    (as,bs) = weave a b

    [a1,a2,a3] = map r2 [(2, 4), (4,-2), (6, 2)]
    [b1,b2,b3] = map r2 [(2,-4), (4, 2), (6,-2)]

weave a b = go a b [] []
  where
    go a b as bs =
        case sortBy (comparing (view _1)) $ segmentSegment 0.01 a b of
            [] -> (reverse (a:as), reverse (b:bs))
            ((ta,_,_):_) -> let (a',a'') = splitAround 0.1 ta a
                            in  go b a'' bs (a':as)

splitAround r t p = ( adjust a (opts & adjSide .~ End)
                    , adjust b (opts & adjSide .~ Start)
                    )
  where
    (a,b) = splitAtParam p t
    opts = def & adjMethod .~ ByAbsolute (-r)

main = mainWith (d # pad 1.1)

-- segmentSegment
--   :: (Floating n, Ord n) =>
--        n -> FixedSegment V2 n -> FixedSegment V2 n -> [(n, n, P2 n)]
--
d' :: Diagram B
d' =   mconcat cs # fc purple # lw 0
   <> strokeT    a # lc blue
   <> strokeLocT b # lc red
  where
    a = fromSegments [bézier3 a1 a2 a3]
    b = fromSegments [bézier3 b1 b2 b3] `at` (0 ^& 2) 

    [a1,a2,a3] = map r2 [(2, 4), (4,-2), (6, 2)]
    [b1,b2,b3] = map r2 [(2,-4), (4, 2), (6,-2)]

    cs = map mkCircle $ intersectPoints a b

    mkCircle p = circle 0.05 # moveTo p


fs :: FixedSegment V2 Double
fs = mkFixedSeg $ bézier3 a1 a2 a3 `at` (0 ^& 0)
[a1,a2,a3] = map r2 [(2, 4), (4,-2), (6, 2)]

main' = defaultMain (pad 1.1 (f fs) === (f a # lc red <> f b # lc blue))
  where
    (a,b) = splitAround 1 0.5 fs
    f = strokeP . toPath

