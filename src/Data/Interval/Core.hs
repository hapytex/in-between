{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Data.Interval.Core where

class HasMembershipCheck i a | i -> a where
  isAnElementOf :: a -> i -> Bool
  isAnElementOf x = not . isNotAnElementOf x

  isNotAnElementOf :: a -> i -> Bool
  isNotAnElementOf x = not . isAnElementOf x

  {-# MINIMAL isAnElementOf | isNotAnElementOf #-}

class ShowInterval i where
  showInterval :: i -> String
  showInterval = flip (showInterval' 0) ""

  showInterval' :: Int -> i -> ShowS
  showInterval' _ = (++) . showInterval
  {-# MINIMAL showInterval | showInterval' #-}

class IntervalMap i a b | i -> a, i -> b where
  intervalLookup :: i -> a -> b
  intervalLookup = intervalLookupWith (:) []

  intervalLookupWith :: (b -> c -> c) -> c -> i -> a -> c
  intervalLookupWith f z i = foldr f z . intervalLookup i

(∈) :: HasMembershipCheck i a => a -> i -> Bool
(∈) = isAnElementOf

(∋) :: HasMembershipCheck i a => i -> a -> Bool
(∋) = flip isAnElementOf

(∉) :: HasMembershipCheck i a => a -> i -> Bool
(∉) = isNotAnElementOf

(∌) :: HasMembershipCheck i a => i -> a -> Bool
(∌) = flip isNotAnElementOf
