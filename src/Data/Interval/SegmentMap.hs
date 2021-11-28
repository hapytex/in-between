{-# LANGUAGE DeriveFunctor #-}

module Data.Interval.SegmentMap where

import Data.Interval.Elementary(ElementaryInterval)

data SegmentTree a b = Node a [(ElementaryInterval a, b)] (SegmentTree a b) (SegmentTree a b) deriving (Functor, Show)

fromList :: Ord a => [(ElementaryInterval a, b)] -> SegmentTree a b
fromList = undefined
