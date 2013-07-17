import Common

import qualified Vector      as WayVector
import qualified Vector2     as WayVector2
import qualified HandFissed  as WayFissed
import qualified HandFissed2 as WayFissed2
import qualified HandFused   as WayFused
import qualified HandFused2  as WayFused2

import System.Environment (getArgs)
import qualified Data.Vector.Unboxed         as V
import Data.Array.Repa.IO.Timing

main
 = do   ss <- getArgs
        case ss of
         [way, rays, tris]
          -> go way (read rays) (read tris)
         _
          -> help

go :: String -> Int -> Int -> IO ()
go way nrays ntris
 | Just wayF <- lookup way ways
 = do   let rays = V.generate nrays gray
            tris = V.generate ntris gtri
        rays `seq` tris `seq` return ()
        (o,t) <- time $ let v = wayF idmtx rays tris
                        in  v `seq` return v
        putStr	$ prettyTime t

 | otherwise
 = help


gray :: Int -> Ray
gray i = ((0, 0, 0), (d, d, d))
 where
  d = fromIntegral i

gtri :: Int -> Tri
gtri i = ( (d,0,0), (0,d,0), (0,0,d) )
 where
  d = fromIntegral i

help :: IO ()
help = putStrLn ("fission <" ++ sways ++ "> rays tris")
 where
  sways = foldl1 (\x y -> x ++ "|" ++ y)
        $ map fst ways

type Way = Matrix44
         -> V.Vector Ray
         -> V.Vector Tri
         -> V.Vector Elt

ways :: [(String,Way)]
ways = [ ("fissed", WayFissed.cull)
       , ("fissed2", WayFissed2.cull)
       , ("fused" ,  WayFused.cull)
       , ("fused2",  WayFused2.cull)
       , ("vector", WayVector.cull) 
       , ("vector2",WayVector2.cull) 
       ]

