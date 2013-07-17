{-# LANGUAGE BangPatterns #-}
module HandFused2 where

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST (runST)

import Common


import GHC.Exts ( SpecConstrAnnotation(..) )
data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

cull :: Matrix44 -- ^ Projection matrix
     -> V.Vector Ray
     -> V.Vector Tri
     -> V.Vector Elt
cull !mtx !rays !tris
 = runST
 $ do   !u  <- M.unsafeNew nrays
        !ts <- M.unsafeNew ntris
        fill_tris SPEC 0 ts
        go_rays SPEC 0 u ts
        V.unsafeFreeze u
 where
  !nrays = V.length rays
  !ntris = V.length tris


  {-# INLINE fill_tris #-}
  fill_tris SPEC !ix !ts
   | ix < ntris
   = do let !t  = V.unsafeIndex tris ix
            !t' = cTriP_Tri t
        M.unsafeWrite ts ix t'
        fill_tris SPEC (ix+1) ts
   
   | otherwise
   =    return ()


  {-# INLINE go_rays #-}
  go_rays SPEC !ix !u !ts
   | ix < nrays
   = do let !r = V.unsafeIndex rays ix
        !d <- get_dist ts r
        M.unsafeWrite u ix d
        go_rays SPEC (ix+1) u ts

   | otherwise
   =    return ()


  {-# INLINE get_dist #-}
  get_dist !ts !ray
   = do let !r' = mrmul mtx ray
        go_tris SPEC ts r' 0 arbitraryLargeElt


  {-# INLINE go_tris #-}
  go_tris SPEC !ts !ray !ix !acc
   | ix < ntris
   = do !t   <- M.unsafeRead ts ix
        if   inside ray t
        then go_tris SPEC ts ray (ix+1) (acc `min` cast ray t)
        else go_tris SPEC ts ray (ix+1)  acc

   | otherwise
   =    return acc

