module Availer.IntervalError where

data IntervalError a b
  = EndBeforeStart a a
  | NegativeLength b