module Cleanable where

class Cleanable a where
  clean :: a -> a
