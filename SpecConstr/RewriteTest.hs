{-# LANGUAGE MagicHash #-}
module RewriteTest where
import GHC.Exts
{-# RULES
  "test" forall a.
    bogus (Right (I# a)) = 0
  #-}

{-# NOINLINE bogus #-}
bogus :: Either Int Int -> Int
bogus (Right i) = i - 5
bogus (Left i) = i + 5

{-# INLINE bogus_right #-}
bogus_right :: Either Int Int
bogus_right = Right bogus_right_i

{-# INLINE bogus_right_i #-}
bogus_right_i :: Int
bogus_right_i = 5


blah = bogus bogus_right
