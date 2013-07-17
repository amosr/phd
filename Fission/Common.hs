module Common
       ( Elt, Vec, Matrix44
       , Ray, Tri, TriP, Plane
       , idmtx
       , mvmul
       , mrmul
       , inside
       , cTriP_Tri
       , cast
       , arbitraryLargeElt)
where

type Elt = Double
type Vec = (Elt,Elt,Elt)

type Matrix44 = ((Elt,Elt,Elt,Elt)
                ,(Elt,Elt,Elt,Elt)
                ,(Elt,Elt,Elt,Elt)
                ,(Elt,Elt,Elt,Elt))

{-# INLINE idmtx #-}
idmtx :: Matrix44
idmtx = ((1, 0, 0, 0)
        ,(0, 1, 0, 0)
        ,(0, 0, 1, 0)
        ,(0, 0, 0, 1))


type Ray = (Vec,Vec)
type Pluecker = (Vec,Vec)
type Tri = (Vec,Vec,Vec)

-- Normal and distance of normal from (0,0,0)
type Plane = (Vec, Elt)
-- triangle with plücker stuff precalculated
type TriP= (Pluecker,Pluecker,Pluecker, Plane)



{-# INLINE mvmul #-}
mvmul :: Matrix44 -> Vec -> Vec
mvmul ((a,b,c,d)
      ,(e,f,g,h)
      ,(i,j,k,l)
      ,(m,n,o,p))

       (x,y,z)
 = ( x*a + y*e + z*i + w*m
   , x*b + y*f + z*j + w*n 
   , x*c + y*g + z*k + w*o
   )
 where
  w = 1

{-# INLINE mrmul #-}
mrmul :: Matrix44 -> Ray -> Ray
mrmul m (p,q) = (mvmul m p, mvmul m q)

-- Get the pluecker coordinates of each edge and the plane, for later
{-# INLINE cTriP_Tri #-}
cTriP_Tri :: Tri -> TriP
cTriP_Tri t@(a,b,c)
 = (p a b, p b c, p c a, planeOfTriangle t)
 where
  p a b = plueckerOfLine (a,b)

{-# INLINE cast #-}
cast :: Ray -> TriP -> Elt
cast r (_,_,_,p)
 = lineOnPlane r p

-- Check if ray will ever end inside all three edge planes
{-# INLINE inside #-}
inside :: Ray -> TriP -> Bool
inside p (a,b,c,_)
 = let pr l = projectPluecker2 p l
       a'   = pr a < 0
       b'   = pr b < 0
       c'   = pr c < 0
   in  (a' && b' && c') || not (a' || b' || c')


{-# INLINE dot #-}
dot :: Vec -> Vec -> Elt
dot (u,v,w) (x,y,z) = u*x + v*y + w*z

{-# INLINE cross #-}
cross :: Vec -> Vec -> Vec
cross (u,v,w) (x,y,z) = ( v * z - w * y
                        , w * x - u * z
                        , u * y - v * x)

{-# INLINE over1 #-}
over1 :: (Elt -> Elt) -> Vec -> Vec
over1 f (x,y,z) = (f x, f y, f z)

{-# INLINE over2 #-}
over2 :: (Elt -> Elt -> Elt) -> Vec -> Vec -> Vec
over2 f (u,v,w) (x,y,z) = (f u x, f v y, f w z)

{-# INLINE mag #-}
mag a = sqrt (a `dot` a)

{-# INLINE norm #-}
norm a = over1 (/m) a
 where m = mag a

{-# INLINE vsub #-}
vadd = over2 (+)
vsub = over2 (-)

{-# INLINE vsmul #-}
vsmul v s = over1 (*s) v

-- Convert a line into pluecker coordinates
{-# INLINE plueckerOfLine #-}
plueckerOfLine :: Ray -> Pluecker
plueckerOfLine (p,q) = (q `vsub` p, q `cross` p)

-- Find intersection of a line and plucker, if exists.
-- Otherwise 
{-# INLINE projectPluecker2 #-}
projectPluecker2 :: Pluecker -> Pluecker -> Elt
projectPluecker2 (u1,v1) (u2,v2) = (u1 `dot` v2) + (u2 `dot` v1)


-- Get plane from triangle, eg for projection
{-# INLINE planeOfTriangle #-}
planeOfTriangle :: Tri -> Plane
planeOfTriangle (a,b,c)
 = let n  = (a `vsub` b) `cross` (c `vsub` b)
       n' = norm n
       d  = negate (n' `dot` b)
   in  (n', d)

-- should this return Maybe Elt?
{-# INLINE lineOnPlane #-}
lineOnPlane :: Ray -> Plane -> Elt
lineOnPlane (p,q) (n,d)
 = let d1 = (n `dot` p) + d
       d2 = (n `dot` q) + d
   in  if   d1 == d2
       then arbitraryLargeElt -- meh. big. fail.
       else d1 / (d1 - d2)


{-
{-# INLINE rotateY #-}
rotateY :: Elt -> Matrix3
rotateY d
 =((         cos d,  0,  sin d)
  ,(             0,  1,      0)
  ,(negate (sin d),  0,  cos d))

{-# INLINE rotateX #-}
rotateX :: Elt -> Matrix3
rotateX d
 =((             1,  0,      0)
  ,(         0, cos d,  sin d)
  ,(         0, negate (sin d),  cos d))


{-# INLINE trans_disp #-}
trans_disp :: Matrix3 -> Vec3 -> Vec3
trans_disp m v
 = (mvmul m (v `vsub` (0,0,0))) `vadd` (0,0,30)
-}


{-# INLINE arbitraryLargeElt #-}
arbitraryLargeElt :: Elt
arbitraryLargeElt = 1e100

