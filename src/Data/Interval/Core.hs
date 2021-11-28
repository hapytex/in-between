{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Data.Interval.Core where

class HasMembershipCheck i a | i -> a where
    isAnElementOf :: a -> i -> Bool
    isAnElementOf x = not . isNotAnElementOf x

    isNotAnElementOf :: a -> i -> Bool
    isNotAnElementOf x = not . isAnElementOf x

    {-# MINIMAL isAnElementOf | isNotAnElementOf #-}

(∈) :: HasMembershipCheck i a => a -> i -> Bool
(∈) = isAnElementOf

(∋) :: HasMembershipCheck i a => i -> a -> Bool
(∋) = flip isAnElementOf

(∉) :: HasMembershipCheck i a => a -> i -> Bool
(∉) = isNotAnElementOf

(∌) :: HasMembershipCheck i a => i -> a -> Bool
(∌) = flip isNotAnElementOf
