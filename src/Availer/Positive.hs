module Availer.Positive where

class Positive b where
  isPositive :: b -> Bool
