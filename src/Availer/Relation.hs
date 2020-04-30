module Availer.Relation
  ( Relation(..)
  , relate
  , invert
  ) where

-- lens
import Control.Lens

import Availer.Boundary
import Availer.Interval

-- | describes how two intervals @x@ and @y@ can be related.
-- See [Allen's interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra)
data Relation
  -- | union is equal to both (= in Allen's notation)
  = Equal
  -- | union is equal to @x@ but bigger than @y@, @start x = start y@, @end x != end y@ (s)
  | Starts
  -- | union is equal to @x@ but bigger than @y@, @start x != start y@, @end x = end y@ (f)
  | Finishes
  -- | union is equal to @x@ but bigger than @y@, @start x != start y@, @end x != end y@ (d)
  | During
  -- | union is bigger than @y@ and equal to @y@, @start x = start y@, @end x != end y@ (si)
  | StartedBy
  -- | union is bigger than @x@ and equal to @y@, @start x != start y@, @end x = end y@ (fi)
  | FinishedBy
  -- | union is bigger than @x@ and equal to @y@, @start x != start y@, @end x != end y@ (di)
  | Contains
  -- | union is bigger than both, union is two intervals, @start x < start y@ (<)
  | Before
  -- | union is bigger than both, union is two intervals, @start x > start y@ (>)
  | After
  -- | union is bigger than both, union is one interval, intersection is empty, @start x < start y@ (m)
  | JustBefore
  -- | union is bigger than both, union is one interval, intersection is empty, @start x < start y@ (mi)
  | JustAfter
  -- | union is bigger than both, union is one interval, intersection is not empty, @start x < start y@ (o)
  | Overlaps
  -- | union is bigger than both, union is one interval, intersection is not empty, @start x > start y@ (oi)
  | OverlappedBy
  deriving (Eq, Show)

eqJust :: Eq a => Maybe a -> Maybe a -> Bool
eqJust (Just a) (Just b) = a == b
eqJust _        _        = False

-- | Computes how two intervals are related according to the @`Relation`@ classification
relate :: Ord a => Interval a -> Interval a -> Relation
relate interval1 interval2 =
  case (interval1 `intersection` interval2 == interval1, interval1 `intersection` interval2 == interval2) of
    (True , True ) -> Equal
    (True , False) | eqJust (preview start interval1) (preview start interval2) -> Starts
                   | eqJust (preview end   interval1) (preview end   interval2) -> Finishes
                   | otherwise                                                  -> During
    (False, True ) | eqJust (preview start interval1) (preview start interval2) -> StartedBy
                   | eqJust (preview end   interval1) (preview end   interval2) -> FinishedBy
                   | otherwise                                                  -> Contains
    (False, False) -> case ( interval1 `union` interval2
                           , (StartBoundary <$> (preview start interval1)) < (StartBoundary <$> (preview start interval2))
                           , isEmpty (interval1 `intersection` interval2)) of
      (Left  _, True , True ) -> JustBefore
      (Left  _, True , False) -> Overlaps
      (Left  _, False, True ) -> JustAfter
      (Left  _, False, False) -> OverlappedBy
      (Right _, True , _    ) -> Before
      (Right _, False, _    ) -> After

-- | inverts a relation, such that @'invert' ('relate' x y) = 'relate' y x@
invert :: Relation -> Relation
invert relation = case relation of
  After        -> Before
  Before       -> After
  Contains     -> During
  During       -> Contains
  Equal        -> Equal
  FinishedBy   -> Finishes
  Finishes     -> FinishedBy
  JustBefore   -> JustAfter
  JustAfter    -> JustBefore
  OverlappedBy -> Overlaps
  Overlaps     -> OverlappedBy
  StartedBy    -> Starts
  Starts       -> StartedBy
