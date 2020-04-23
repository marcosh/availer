{-# LANGUAGE TemplateHaskell #-}

module Availer.Boundary where

import Control.Lens.TH (makeLenses)

data IsIncluded
  = Included
  | Excluded
  deriving (Eq)

data Boundary a = Boundary
  { _boundaryValue :: a
  , _isIncluded :: IsIncluded
  }
  deriving (Eq)

makeLenses ''Boundary

-- | notice that this `Ord` instance is not antisymmetric
instance Ord a => Ord (Boundary a) where
  compare (Boundary a1 _) (Boundary a2 _) = compare a1 a2
