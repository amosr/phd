module Blowup3 where

{-# INLINE sndlast #-}
sndlast :: Maybe (a, Maybe a) -> a -> Maybe (a, Maybe a)
sndlast Nothing               = \a -> Just (a,   Nothing)
sndlast (Just (a', Nothing )) = \a -> Just (a',  Just a)
sndlast (Just (a', Just a'')) = \a -> Just (a'', Just a)
{-
sndlast outer a =
 case outer of
  Nothing -> Just (a,   Nothing)
  Just (a',inner) -> sndlast_2 inner a'
 where
  sndlast_2 i a'
   = case i of
      Nothing  -> Just (a',  Just a)
      Just a'' -> Just (a'', Just a)
-}
{-
sndlast Nothing b = Just (b, Nothing)
sndlast (Just a) b = Just a
-}

{-# INLINE fold #-}
fold :: (b -> a -> b) -> b -> [a] -> b
fold k z xs' = go xs' z
 where
  go []     v = v
  go (x:xs) v = go xs (k v x)

test :: [Int] -> Maybe Int
test xs
 = case fold sndlast Nothing xs of
    Nothing    -> Nothing
    Just (a,_) -> Just a

