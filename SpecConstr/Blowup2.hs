{-# LANGUAGE ExistentialQuantification, BangPatterns #-}
module Blowup2 where

beef :: [Int] -> Int -> [Int]
beef str 0 = str
beef (str@[a,b,c,d,e,f,g,h]) n = str ++ beef str (n-1)
--beef (str@[a]) n = str ++ beef str (n-1)
--beef (str@[a,b]) n = str ++ beef str (n-1)
--beef (str@[a,b,c]) n = str ++ beef str (n-1)
--beef (str@(a:as)) n = str ++ beef str (n-1)
beef str n = beef str (n-1) ++ str
