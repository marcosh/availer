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

-- | compare two boundaries as starting point of an interval
compareStart :: Ord a => Boundary a -> Boundary a -> Ordering
compareStart (Boundary a1 Included) (Boundary a2 Excluded) = compare a1 a2 <> LT
compareStart (Boundary a1 Excluded) (Boundary a2 Included) = compare a1 a2 <> GT
compareStart (Boundary a1 _       ) (Boundary a2 _       ) = compare a1 a2

-- | compare two boundaries as ending point of an interval
compareEnd :: Ord a => Boundary a -> Boundary a -> Ordering
compareEnd (Boundary a1 Included) (Boundary a2 Excluded) = compare a1 a2 <> GT
compareEnd (Boundary a1 Excluded) (Boundary a2 Included) = compare a1 a2 <> LT
compareEnd (Boundary a1 _       ) (Boundary a2 _       ) = compare a1 a2