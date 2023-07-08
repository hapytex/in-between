{-# LANGUAGE DeriveFunctor, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Data.Interval.Elementary where

import Data.Interval.Core(HasMembershipCheck(isAnElementOf), ShowInterval(showInterval'))

-- | One of the parts of an elementary bound. The 'ElementaryBound' can be applied to the
-- lowerbound of the interval, or the upperbound of the interval.
data ElementaryBound a
  = Open a  -- ^ An open bound a that describes that the given value is /not/ an element of the interval.
  | Closed a  -- ^ A closed bound that describes that the given value is an element of the interval.
  | Infinity  -- ^ 'Infinity' is used to describe that that part of the bound puts no constraint on the interval.
  deriving (Eq, Functor, Show)

-- | An interval that is /only/ bounded by a /lowerbound/ or /upperbound/.
data Semibound a
  = Lower (ElementaryBound a)  -- ^ An interval only bounded by the /lowerbound/.
  | Upper (ElementaryBound a)  -- ^ An interval only bounded by the /upperbound/.
  deriving (Eq, Functor, Show)

class IsElementaryInterval i a | i -> a where
  toElementaryInterval :: i -> ElementaryInterval a
  {-# MINIMAL toElementaryInterval #-}

instance IsElementaryInterval (ElementaryInterval a) a where
  toElementaryInterval = id

instance IsElementaryInterval (Semibound a) a where
  toElementaryInterval (Lower l) = ElementaryInterval l Infinity
  toElementaryInterval (Upper u) = ElementaryInterval Infinity u

data ElementaryInterval a
  = ElementaryInterval (ElementaryBound a) (ElementaryBound a)
  deriving (Eq, Functor, Show)

toBound :: ElementaryBound a -> [a] -> [a]
toBound (Open a) = (a:)
toBound (Closed a) = (a:)
toBound Infinity = id

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
  isAnElementOf x = go
    where go (Lower l) = leftCheck l x
          go (Upper u) = rightCheck u x

instance Show a => ShowInterval (ElementaryInterval a) where
  showInterval' _ (ElementaryInterval ga gb) = fl ga . (',' :) . fr gb
    where fl (Open v) = ('(' :) . showsPrec 10 v
          fl (Closed v) = ('[' :) . showsPrec 10 v
          fl Infinity = ('(' : ) . ('−' : ) . ('∞' :)
          fr (Open v) = showsPrec 10 v .  (')' :)
          fr (Closed v) = showsPrec 10 v .  (']' :)
          fr Infinity = ('+' :) . ('∞' :) .  (')' :)

instance Show a => ShowInterval (Semibound a) where
  showInterval' p = showInterval' p . toElementaryInterval
