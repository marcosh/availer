{-# LANGUAGE MultiParamTypeClasses #-}

module Availer.Action where

class Action a b where
  add        :: a -> b -> a
  difference :: a -> a -> b
