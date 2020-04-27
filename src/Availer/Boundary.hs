{-# LANGUAGE TemplateHaskell #-}

module Availer.Boundary where

import Control.Lens.TH (makeLenses)

data IsIncluded
  = Included
  | Excluded
  deriving (Eq, Show)

data Boundary a = Boundary
  { _boundaryValue :: a
  , _isIncluded :: IsIncluded
  }
  deriving (Eq, Show)

makeLenses ''Boundary

-- | notice that this `Ord` instance is not antisymmetric
instance Ord a => Ord (Boundary a) where
  compare (Boundary a1 _) (Boundary a2 _) = compare a1 a2

newtype StartBoundary a = StartBoundary (Boundary a)
  deriving (Eq, Show)

instance Ord a => Ord (StartBoundary a) where
  compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Excluded)) = compare a1 a2 <> LT
  compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Included)) = compare a1 a2 <> GT
  compare (StartBoundary (Boundary a1 _       )) (StartBoundary (Boundary a2 _       )) = compare a1 a2

newtype EndBoundary a = EndBoundary (Boundary a)
  deriving (Eq, Show)

instance Ord a => Ord (EndBoundary a) where
  compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Excluded)) = compare a1 a2 <> GT
  compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Included)) = compare a1 a2 <> LT
  compare (EndBoundary (Boundary a1 _       )) (EndBoundary (Boundary a2 _       )) = compare a1 a2

newtype GTBoundary a = GTBoundary (Boundary a)
  deriving (Eq, Show)

-- | notice how `compare (GTBoundary (Boundary a Excluded)) (GTBoundary (Boundary a Excluded))` equals `GT` and not `EQ`
instance Ord a => Ord (GTBoundary a) where
  compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Included)) = compare a1 a2
  compare (GTBoundary (Boundary a1 _       )) (GTBoundary (Boundary a2 _       )) = compare a1 a2 <> GT

newtype LTBoundary a = LTBoundary (Boundary a)
  deriving (Eq, Show)

-- | notice how `compare (LTBoundary (Boundary a Excluded)) (LTBoundary (Boundary a Excluded))` equals `LT` and not `EQ`
instance Ord a => Ord (LTBoundary a) where
  compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Included)) = compare a1 a2
  compare (LTBoundary (Boundary a1 _       )) (LTBoundary (Boundary a2 _       )) = compare a1 a2 <> LT
