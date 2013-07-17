{-# LANGUAGE BangPatterns #-}
module HandFused where

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST (runST)

import Common


cull :: Matrix44 -- ^ Projection matrix
     -> V.Vector Ray
     -> V.Vector Tri
     -> V.Vector Elt
cull !mtx !rays !tris
 = runST
 $ do   !u <- M.unsafeNew (V.length rays)
        go_rays 0 u
        V.unsafeFreeze u
 where
  nrays = V.length rays
  ntris = V.length tris

  {-# INLINE go_rays #-}
  go_rays !ix !u
   | ix < nrays
   = do let !r = V.unsafeIndex rays ix
            !d = get_dist r
        M.unsafeWrite u ix d
        go_rays (ix+1) u

   | otherwise
   =    return ()


  {-# INLINE get_dist #-}
  get_dist !ray
   = let !r' = mrmul mtx ray
     in  go_tris r' 0 arbitraryLargeElt


  {-# INLINE go_tris #-}
  go_tris !ray !ix !acc
   | ix < ntris
   = let !t   = V.unsafeIndex tris ix
         !t'  = cTriP_Tri t
     in  if   inside ray t'
         then go_tris ray (ix+1) (acc `min` cast ray t')
         else go_tris ray (ix+1)  acc

   | otherwise
   = acc

