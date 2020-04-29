module Availer.Relation where

-- | describes how two intervals `x` and `y` can be related
-- | see https://en.wikipedia.org/wiki/Allen%27s_interval_algebra
data Relation
  -- | union is equal to both (= in Allen's notation)
  = Equal
  -- | union is equal to x but bigger than y, start x = start y, end x != end y (s)
  | Starts
  -- | union is equal to x but bigger than y, start x != start y, end x = end y (f)
  | Finishes
  -- | union is equal to x but bigger than y, start x != start y, end x != end y (d)
  | During
  -- | union is bigger than y and equal to y, start x = start y, end x != end y (si)
  | StartedBy
  -- | union is bigger than x and equal to y, start x != start y, end x = end y (fi)
  | FinishedBy
  -- | union is bigger than x and equal to y, start x != start y, end x != end y (di)
  | Contains
  -- | union is bigger than both, union is two intervals, start x < start y (<)
  | Before
  -- | union is bigger than both, union is two intervals, start x > start y (>)
  | After
  -- | union is bigger than both, union is one interval, intersection is empty, start x < start y (m)
  | JustBefore
  -- | union is bigger than both, union is one interval, intersection is empty, start x < start y (mi)
  | JustAfter
  -- | union is bigger than both, union is one interval, intersection is not empty, start x < start y (o)
  | Overlaps
  -- | union is bigger than both, union is one interval, intersection is not empty, start x > start y (oi)
  | OverlappedBy
