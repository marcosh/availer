{-# LANGUAGE TemplateHaskell #-}

module Availer.Boundary where

import Control.Lens.TH (makeLenses)

-- | Flag to describe whether a boundary should be considered included in the interval or not
data IsIncluded
  = Included
  | Excluded
  deriving (Eq, Show)

-- | Boundary of an interval, containing the value of the boundary and whether it is included or not in the interval.
data Boundary a = Boundary
  { _boundaryValue :: a
  , _isIncluded :: IsIncluded
  }
  deriving (Eq, Show)

makeLenses ''Boundary

-- | notice that this `Ord` instance is not antisymmetric
instance Ord a => Ord (Boundary a) where
  compare (Boundary a1 _) (Boundary a2 _) = compare a1 a2

-- | newtype used to compare two starting boundaries
newtype StartBoundary a = StartBoundary (Boundary a)
  deriving (Eq, Show)

instance Ord a => Ord (StartBoundary a) where
  compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Excluded)) = compare a1 a2 <> LT
  compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Included)) = compare a1 a2 <> GT
  compare (StartBoundary (Boundary a1 _       )) (StartBoundary (Boundary a2 _       )) = compare a1 a2

-- | newtype used to compare two ending boundaries
newtype EndBoundary a = EndBoundary (Boundary a)
  deriving (Eq, Show)

instance Ord a => Ord (EndBoundary a) where
  compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Excluded)) = compare a1 a2 <> GT
  compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Included)) = compare a1 a2 <> LT
  compare (EndBoundary (Boundary a1 _       )) (EndBoundary (Boundary a2 _       )) = compare a1 a2

-- | newtype used to compare two boundaries where the second is always greater than the first if they have the same
-- value and they are not both included
newtype GTBoundary a = GTBoundary (Boundary a)
  deriving (Eq, Show)

-- | notice how @'compare' ('GTBoundary' ('Boundary' a 'Excluded')) ('GTBoundary' ('Boundary' a 'Excluded'))@ equals @'GT'@ and not @'EQ'@
instance Ord a => Ord (GTBoundary a) where
  compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Included)) = compare a1 a2
  compare (GTBoundary (Boundary a1 _       )) (GTBoundary (Boundary a2 _       )) = compare a1 a2 <> GT

-- | newtype used to compare two boundaries where the second is always smaller than the first if they have the same
-- value and they are not both included
newtype LTBoundary a = LTBoundary (Boundary a)
  deriving (Eq, Show)

-- | notice how @compare ('LTBoundary' ('Boundary' a 'Excluded')) ('LTBoundary' ('Boundary' a 'Excluded'))@ equals @'LT'@ and not @'EQ'@
instance Ord a => Ord (LTBoundary a) where
  compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Included)) = compare a1 a2
  compare (LTBoundary (Boundary a1 _       )) (LTBoundary (Boundary a2 _       )) = compare a1 a2 <> LT

-- | newtype used to check wheter the union of an interval with the first interval as ending boundary and an interval
-- having the second interval as starting boundary form a single interval
newtype UnionBoundary a = UnionBoundary (Boundary a)
  deriving (Eq, Show)

-- | notice how @'compare' ('UnionBoundary' ('Boundary' a 'Excluded')) ('UnionBoundary' ('Boundary' a 'Excluded'))@ equals @'LT'@ and not @'EQ'@
instance Ord a => Ord (UnionBoundary a) where
  compare (UnionBoundary (Boundary a1 Excluded)) (UnionBoundary (Boundary a2 Excluded)) = compare a1 a2 <> LT
  compare (UnionBoundary (Boundary a1 _       )) (UnionBoundary (Boundary a2 _       )) = compare a1 a2
