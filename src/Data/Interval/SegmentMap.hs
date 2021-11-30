{-# LANGUAGE DeriveFunctor #-}

module Data.Interval.SegmentMap where

import Control.Monad(join)

import Data.Interval.Elementary(ElementaryInterval)

data SegmentMap a b = Node a [(ElementaryInterval a, b)] (SegmentMap a b) (SegmentMap a b) deriving (Functor, Show)

fromList' :: Ord a => [ElementaryInterval a] -> SegmentMap a (ElementaryInterval a)
fromList' = fromList . map (join (,))

fromList :: Ord a => [(ElementaryInterval a, b)] -> SegmentMap a b
fromList = undefined

getListItems :: Ord a => [(ElementaryInterval a, b)] => [(a, (SemiBound a, b))]
getListItems = sortOn fst . foldr toBounds []
  where toBounds ((ElementaryInterval la, lr), y) = gol la . gor lr
        where gol Infinity = id
              gol x = ((x, (Lower x, y)) :)
              gor Infinity = id
              gor x = ((x, (Upper x, y)) :)
