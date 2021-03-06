module Vector2 where

import qualified Data.Vector.Unboxed as V

import Common

cull :: Matrix44 -- ^ Projection matrix
     -> V.Vector Ray
     -> V.Vector Tri
     -> V.Vector Elt
cull mtx rays tris
 = dists
 where
  -- Noinline so it's reified in memory instead of fused
  {-# NOINLINE tris' #-}
  tris'
   = V.map cTriP_Tri
   $ tris

  {-# INLINE dist #-}
  dist ray
   = V.foldl min arbitraryLargeElt
   $ V.map    (cast ray)
   $ V.filter (inside ray) 
   $ tris'

  {-# INLINE dists #-}
  dists
   = V.map dist
   $ V.map (mrmul mtx) rays

