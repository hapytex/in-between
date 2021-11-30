{-# LANGUAGE DeriveFunctor #-}

module Data.Interval.SegmentMap where

import Control.Monad(join)

import Data.Interval.Elementary(ElementaryBound(Open, Closed, Infinity), ElementaryInterval(ElementaryInterval), Semibound(Lower, Upper))
import Data.List(sortOn)

data SegmentMap a b = Node a [(ElementaryInterval a, b)] (SegmentMap a b) (SegmentMap a b) deriving (Functor, Show)

fromList' :: Ord a => [ElementaryInterval a] -> SegmentMap a (ElementaryInterval a)
fromList' = fromList . map (join (,))

fromList :: Ord a => [(ElementaryInterval a, b)] -> SegmentMap a b
fromList = undefined

getListItems :: Ord a => [(ElementaryInterval a, b)] -> [(a, (Semibound a, b))]
getListItems = sortOn fst . foldr toBounds []
  where toBounds (ElementaryInterval la lr, y) = gol la . gor lr
          where go _ Infinity = id
                go f v@(Open x) = ((x, (f v, y)) :)
                go f v@(Closed x) = ((x, (f v, y)) :)
                gol = go Lower
                gor = go Upper
