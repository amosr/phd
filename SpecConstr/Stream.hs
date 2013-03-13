-- Very simplified Stream functions for showing SpecConstr example.
--
{-# LANGUAGE ExistentialQuantification, BangPatterns #-}
module Stream where

import GHC.Exts ( SpecConstrAnnotation(..) )

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}


data Stream a = forall s. Stream (s -> Step a s) s

data Step a s = Done | Skip s | Yield a s

{-# INLINE unstream #-}
unstream :: Stream a -> [a]
unstream (Stream next s0)
 = unfold s0
 where
  {-# INLINE unfold #-}
  unfold s
   = case next s of
      Done       ->     []
      Skip s'    ->     unfold s'
      Yield a s' -> a : unfold s'
{-
 = unfold SPEC s0
 where
  {-# INLINE unfold #-}
  unfold SPEC s
   = case next s of
      Done       ->     []
      Skip s'    ->     unfold SPEC s'
      Yield a s' -> a : unfold SPEC s'
-}


{-# INLINE zipS #-}
zipS :: Stream a -> Stream b -> Stream (a,b)
zipS (Stream nextA sA0) (Stream nextB sB0)
 = Stream next (sA0, sB0, Nothing)
 where
  {-# INLINE next #-}
  next (sA, sB, Nothing)
   = case nextA sA of
      Done        -> Done
      Skip sA'    -> Skip (sA', sB, Nothing)
      Yield a sA' -> Skip (sA', sB, Just a)
  next (sA, sB, Just a)
   = case nextB sB of
      Done        -> Done
      Skip sB'    -> Skip (sA, sB', Just a)
      Yield b sB' -> Yield (a,b) (sA, sB', Nothing)


{-# INLINE enumFromToS #-}
enumFromToS :: Int -> Int -> Stream Int
enumFromToS f t
 = Stream next f
 where
  {-# INLINE next #-}
  next i | i < t     = Yield i (i+1)
         | otherwise = Done


{-# INLINE appS #-}
appS :: Stream a -> Stream a -> Stream a
appS (Stream nextA sA0) (Stream nextB sB0)
 = Stream next (Left sA0)
 where
  {-# INLINE next #-}
  next (Left sA)
   = case nextA sA of
      Done        -> Skip    (Right sB0)
      Skip sA'    -> Skip    (Left  sA')
      Yield a sA' -> Yield a (Left  sA')
  next (Right sB)
   = case nextB sB of
      Done        -> Done
      Skip sB'    -> Skip    (Right sB')
      Yield b sB' -> Yield b (Right sB')

