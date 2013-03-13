module Blowup4 where

import GHC.Exts ( SpecConstrAnnotation(..) )

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}
loop :: SPEC -> [Int] -> [Int] -> [Int]
loop SPEC z [] = z
loop SPEC z (x:xs) = loop SPEC (x:z) xs

loop5 :: [Int] -> [Int] -> [Int]
loop5 zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop5 [x] xs'
               (z:zs') -> (x+z) : loop5 (x:z:zs') (xs')

{-
loop4 :: SPEC -> [Int] -> [Int] -> [Int]
loop4 SPEC zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop4 SPEC [x] xs'
               (z:zs') -> (x+z) : loop4 SPEC (x:z:zs') xs'
-}

{-
loop :: SPEC -> [Int] -> [Int] -> [Int]
loop SPEC z [] = z
loop SPEC z (x:xs) = loop SPEC (x:z) xs
-}

{-
-- This causes an infinite loop because it's forcing specialisation.
-- There isn't even any point specialising this...
loop zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> loop (x:zs) xs'
==>
      loop' zs x xs = loop (x:zs) xs
RULE: loop zs (x:xs) = loop' zs x xs
-- But there doesn't seem to be a point in this, since the constructed argument (x:zs) is never destructed.
-- Even if the constructed argument were destructed, would it still specialise infinitely?
--
loop2 zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop2 [x] xs'
               (z:zs') -> (x+z) : loop2 zs' xs'

-- Evaluate:
loop2 [1,2,3] [6,7,8]
= (1+6) : (2+7) : (3+8) : []
loop2 [1,2]   [6,7,8]
= (1+6) : (2+7) : 8 : []
loop2 [1,2]   [6,7,8,9]
= (1+6) : (2+7) : (8+9) : []
...hm

-- Specialise:
loop2 zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop2 [x] xs'
               (z:zs') -> (x+z) : loop2 zs' xs'

RULE: loop2 [x] xs = loop2'1 x xs
loop2'1 z xs = case xs of [] -> [z] ; (x:xs') -> (x+z) : loop2 [] xs'

RULE: loop2 [] xs  = loop2'2 xs
loop2'2 xs = case xs of [] -> [] ; (x:xs') -> loop2 [x] xs'


ANOTHER EXAMPLE, but equally useless function.

loop3 zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop3 [x] xs'
               (z:zs') -> (x+z) : loop3 (x:zs') xs'


R: loop3 [x] xs = loop3'1 x xs
loop3'1 z xs = case xs of [] -> [z] ; (x:xs') -> (x+z) : loop3 [x] xs'

R: loop3 (z:zs) xs = loop3'2 z zs xs
loop3'2 z zs xs = case xs of [] -> (z:zs) ; (x:xs') -> (x+z) : loop3 (x:zs) xs'

-- Yes, this is fine because the zs' is "smaller" than input. What about..
--
loop4 zs xs
 = case xs of
   []       -> zs
   (x:xs')  -> case zs of
               []      -> loop4 [x] xs'
               (z:zs') -> (x+z) : loop4 (x:z:zs') xs'

R: loop4 (x:z:zs') xs' = loop4'2 x z zs' xs'
loop4'2 x z zs xs = case xs of [] -> (x:z:zs); (x':xs') -> (x'+x) : loop4 (x':x:z:zs') xs' -- {zs}=x:z:zs. case {zs} of {z:zs'} => {z}=x, {zs'}=z:zs.
R: loop4 (x':x:z:zs') ... (explosion)
-}
