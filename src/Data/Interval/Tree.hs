{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Data.Interval.Tree where

data Balance = Left | Fine | Right deriving (Eq, Ord, Read, Show)
data IntervalTree i a = Node (IntervalTree i a) Balance i (IntervalTree i a) | Leaf a deriving (Eq, Foldable, Functor, Read, Show)

empty :: IntervalTree i (Maybe a)
empty = Leaf Nothing

empty' :: IntervalTree i a
empty' = Leaf (error "No value specified for this value")

setInterval :: BasicInterval i -> IntervalTree i a -> IntervalTree i a
setInterval = undefined
