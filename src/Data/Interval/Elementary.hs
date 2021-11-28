{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.Interval.Elementary where

import Data.Interval.Core(HasMembershipCheck(isAnElementOf), ShowInterval(showInterval'))

data ElementaryBound a
  = Open a
  | Closed a
  | Infinity

data Semibound a
  = Lower (ElementaryBound a)
  | Upper (ElementaryBound a)

data ElementaryInterval a = ElementaryInterval (ElementaryBound a) (ElementaryBound a)

leftCheck :: Ord a => ElementaryBound a -> a -> Bool
leftCheck (Open x) = (x <)
leftCheck (Closed x) = (x <=)
leftCheck Infinity = const True

rightCheck :: Ord a => ElementaryBound a -> a -> Bool
rightCheck (Open x) = (x >)
rightCheck (Closed x) = (x >=)
rightCheck Infinity = const True

instance Ord a => HasMembershipCheck (ElementaryInterval a) a where
  isAnElementOf x (ElementaryInterval l u) = leftCheck l x && rightCheck u x

instance Ord a => HasMembershipCheck (Semibound a) a where
  isAnElementOf x (Lower l) = leftCheck l x
  isAnElementOf x (Upper u) = rightCheck u x

instance Show a => ShowInterval (ElementaryInterval a) where
  showInterval' _ (ElementaryInterval ga gb) = fl ga . (',' :) . fr gb
    where fl (Open v) = ('(' :) . showsPrec 0 v
          fl (Closed v) = ('[' :) . showsPrec 0 v
          fl Infinity = ('(' : ) . ('−' : ) . ('∞' :)
          fr (Open v) = showsPrec 0 v .  (')' :)
          fr (Closed v) = showsPrec 0 v .  (']' :)
          fr Infinity = ('+' :) . ('∞' :) .  (')' :)

instance Show a => ShowInterval (Semibound a) where
  showInterval' p = go
      where go (Lower l) = showInterval' p (ElementaryInterval l Infinity)
            go (Upper u) = showInterval' p (ElementaryInterval Infinity u)
