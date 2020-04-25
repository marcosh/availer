module Availer.Relation where

-- | describes how two intervals `x` and `y` can be related
-- | see https://en.wikipedia.org/wiki/Allen%27s_interval_algebra
data Relation
  -- | empty intersection, union is two intervals, first before second (< in Allen's notation)
  = Before
  -- | empty intersection, union is two intervals, second before first (>)
  | After
  -- | empty intersection, union is one interval, first just before second (m)
  | Meets
  -- | empty intersection, union is one interval, second just before first (mi)
  | MetBy
  -- | non-empty intersection, equal to both intervals (=)
  | Equal
  -- | non-empty intersection, smaller than both intervals, first starts before second (o)
  | Overlaps
  -- | non-empty intersection, smaller that both intervals, second starts before first (oi)
  | OverlappedBy
  -- | non-empty intersection, equal to the first but not the second, same start (s)
  | Starts
  -- | non-empty intersection, equal to the first but not the second, same end (f)
  | Finishes
  -- | non-empty intersection, equal to the first but not the second, different start and end (d)
  | During
  -- | non-empty intersection, equal to the second but not the first, same start (si)
  | StartedBy
  -- | non-empty intersection, equal to the second but not the first, same end (fi)
  | FinishedBy
  -- | non-empty intersection, equal to the second but not the first, different start and end (di)
  | Contains
