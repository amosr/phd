module HandFissed2 where

import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST (runST)
import Control.Monad    (when)

import Common

bufSize = 512

cull :: Matrix44 -- ^ Projection matrix
     -> V.Vector Ray
     -> V.Vector Tri
     -> V.Vector Elt
cull mtx rays tris
 = runST
 $ do   u    <- M.unsafeNew (V.length rays)
        rayB <- M.unsafeNew bufSize
        go_rays 0 u rayB
        V.unsafeFreeze u
 where
  {-# NOINLINE tris' #-}
  tris' = V.map cTriP_Tri tris

  go_rays ix u rayB
   | ix < V.length rays
   = do let ix' = (ix + bufSize) `min` V.length rays
        fill_rays 0 ix ix' rayB
        fill_acc    ix ix' u
        go_tris   0 ix ix' u rayB
        go_rays        ix' u rayB

   | otherwise
   =    return ()


  fill_rays ixB ix ix' rayB
   | ix < ix'
   = do let r  = V.unsafeIndex rays ix
            r' = mrmul mtx r
        M.unsafeWrite rayB ixB r'
        fill_rays (ixB+1) (ix+1) ix' rayB

   | otherwise
   =    return ()


  fill_acc ix ix' u
   | ix < ix'
   = do M.unsafeWrite u ix arbitraryLargeElt
        fill_acc (ix+1) ix' u

   | otherwise
   =    return ()


  go_tris ixT ix ix' u rayB
   | ixT < V.length tris'
   = do let t  = V.unsafeIndex tris' ixT
        go_tri        0 ix ix' u rayB t
        go_tris (ixT+1) ix ix' u rayB

   | otherwise
   =    return ()

  go_tri ixB ix ix' u rayB t
   | ix < ix'
   = do ray <- M.unsafeRead rayB ixB
        when (inside ray t) $ do
          acc <- M.unsafeRead u    ix
          M.unsafeWrite u ix (acc `min` cast ray t)
        go_tri (ixB+1) (ix+1) ix' u rayB t

   | otherwise
   =    return ()

