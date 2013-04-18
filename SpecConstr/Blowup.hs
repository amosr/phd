-- Example of SpecConstr blowing up
-- Compile with -fspec-constr-count=186 or greater and -v
-- to see term size rise from 300 to 11,000 after SpecConstr.
--module Blowup where
import Stream

-- blah :: ([Int], [Int])
blah = unstream d
 where
  -- The state of this stream should be something like
  --
  -- ( Either Int Int
  -- , ( Either Int Int
  --   , ( Either Int Int
  --     , Either Int Int
  --     , Maybe Int )
  --   , Maybe Int )
  -- , Maybe Int )
  --
  -- because each appS (append) corresponds to (Either s_l s_r);
  -- zipS corresponds to (s_l ,s_r, Maybe a); and
  -- enumFromToS corresponds to a single Int.
  --
  -- To get this into a tight loop with no allocation/unboxing,
  -- we would need one function per constructor.
  -- That is about twenty-five copies of the function, including I#.
  -- Looking at the -dverbose-core2core, however, it seems more like hundreds.
  -- (Perhaps even 186 of them)
  d =        (enumFromToS 1 len `appS` enumFromToS 1 len)
      `zipS` (enumFromToS 1 len `appS` enumFromToS 1 len)
      `zipS` (enumFromToS 1 len `appS` enumFromToS 1 len)
--      `zipS` (enumFromToS 1 10 `appS` enumFromToS 1 10)
--      `zipS` (enumFromToS 1 10 `appS` enumFromToS 1 10)
--      `zipS` (enumFromToS 1 10 `appS` enumFromToS 1 10)
  len = 1000
 
main = let x = foldl seq ((0,0),0) blah  in putStrLn (show x)
 -- ( Either Int Int, Either Int Int, Maybe Int )
