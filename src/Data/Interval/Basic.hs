{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Interval.Basic where

import Control.Applicative(liftA2)

data Two a
  = None
  | One a
  | Two a a
  deriving (Eq, Functor, Read, Show)

instance Semigroup a => Semigroup (Two a) where
  None <> _ = None
  _ <> None = None
  One x <> One y = One (x <> y)
  One x <> Two y1 y2 = Two (x <> y1) (x <> y2)
  Two x1 x2 <> One y = Two (x1 <> y) (x2 <> y)
  Two x1 x2 <> Two y1 y2 = Two (x1 <> y1) (x2 <> y2)

instance Monoid a => Monoid (Two a) where
  mempty = One mempty

instance Num a => Num (Two a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = One . fromInteger

instance Fractional a => Fractional (Two a) where
  (/) = liftA2 (/)
  fromRational = One . fromRational

_toFirst :: Two a -> Two a
_toFirst None = None
_toFirst o@(One _) = o
_toFirst (Two x _) = One x

_toSecond :: Two a -> Two a
_toSecond None = None
_toSecond o@(One _) = o
_toSecond (Two _ x) = One x

instance Applicative Two where
  pure = One
  None <*> _ = None
  _ <*> None = None
  One f <*> One x = One (f x)
  One f <*> Two x1 x2 = Two (f x1) (f x2)
  Two f1 f2 <*> One x = Two (f1 x) (f2 x)
  Two f1 f2 <*> Two x1 x2 = Two (f1 x1) (f2 x2)

instance Monad Two where
  return = One
  None >>= _ = None
  One x >>= f = f x
  Two x1 x2 >>= f = go (_toFirst (f x1)) (_toSecond (f x2))
    where go None x = x
          go x None = x
          go (One x) (One y) = Two x y
          go _ _ = error "This should never happen!"

data BoundElement a
  = Excluding a
  | Including a
  | Infinity
  deriving (Eq, Functor, Read, Show)

newtype LowerBound a = LowerBound (BoundElement a) deriving (Eq, Functor, Read, Show)
newtype UpperBound a = UpperBound (BoundElement a) deriving (Eq, Functor, Read, Show)

instance Ord a => Ord (LowerBound a) where
  compare ~(LowerBound l1) ~(LowerBound l2) = _compare' l1 l2

instance Ord a => Semigroup (LowerBound a) where
  (<>) = max

instance Ord a => Monoid (LowerBound a) where
  mempty = LowerBound Infinity

instance Ord a => Ord (UpperBound a) where
  compare ~(UpperBound u1) ~(UpperBound u2) = _compare' u2 u1  -- swapped

instance Ord a => Semigroup (UpperBound a) where
  (<>) = min

instance Ord a => Monoid (UpperBound a) where
  mempty = UpperBound Infinity

_mirror :: BoundElement a -> BoundElement a
_mirror (Including x) = Excluding x
_mirror (Excluding x) = Including x
_mirror x = x

data IntervalRelation
  = Equal
  | Contained
  | Overlap
  | NoOverlap
  deriving (Eq, Ord, Read, Show)

class CheckInterval i a | i -> a where
  isElement :: a -> i -> Bool
  isElement x = not . isNotElement x

  isNotElement :: a -> i -> Bool
  isNotElement x = not . isElement x

  isEmpty :: i -> Bool
  isEmpty = not . isNotEmpty

  isNotEmpty :: i -> Bool
  isNotEmpty = not . isEmpty
  {-# MINIMAL (isElement | isNotElement), (isEmpty | isNotEmpty) #-}

instance Ord a => CheckInterval (LowerBound a) a where
  isElement x (LowerBound l) = lowerConstraint l x
  isEmpty = const False

instance Ord a => CheckInterval (UpperBound a) a where
  isElement x (UpperBound u) = upperConstraint u x
  isEmpty = const False

instance Ord a => CheckInterval (BasicInterval a) a where
  isElement x (BasicInterval l u) = lowerConstraint l x && upperConstraint u x
  isEmpty (BasicInterval (Including l) (Including u)) = u < l
  isEmpty (BasicInterval (BoundValue l) (BoundValue u)) = u <= l
  isEmpty _ = False

pattern BoundValue :: a -> BoundElement a
pattern BoundValue v <- (boundValue -> Just v)

boundValue :: BoundElement a -> Maybe a
boundValue (Including v) = Just v
boundValue (Excluding v) = Just v
boundValue Infinity = Nothing

cataBoundElement :: b -> (a -> b) -> (a -> b) -> BoundElement a -> b
cataBoundElement inf i e = go
  where
    go Infinity = inf
    go (Including v) = i v
    go (Excluding v) = e v

_constr :: (a -> b -> Bool) -> (a -> b -> Bool) -> BoundElement a -> b -> Bool
_constr = cataBoundElement (const True)

lowerConstraint :: Ord a => BoundElement a -> (a -> Bool)
lowerConstraint = _constr (<=) (<)

upperConstraint :: Ord a => BoundElement a -> (a -> Bool)
upperConstraint = _constr (>=) (>)

elementBounded :: BoundElement a -> Bool
elementBounded Infinity = False
elementBounded _ = True

data BasicInterval a
  = BasicInterval (BoundElement a) (BoundElement a)
  deriving (Eq, Read, Show)

instance Ord a => Ord (BasicInterval a) where
  compare b1 b2 = compare l1 l2 <> compare u1 u2
    where ~(l1, u1) = toBounds b1
          ~(l2, u2) = toBounds b2

toBounds :: BasicInterval a -> (LowerBound a, UpperBound a)
toBounds (BasicInterval l u) = (LowerBound l, UpperBound u)

fromBounds :: LowerBound a -> UpperBound a -> BasicInterval a
fromBounds (LowerBound l) (UpperBound u) = BasicInterval l u

intersect :: Ord a => BasicInterval a -> BasicInterval a -> BasicInterval a
intersect b1 b2 = fromBounds (l1 <> l2) (u1 <> u2)
  where ~(l1, u1) = toBounds b1
        ~(l2, u2) = toBounds b2

instance Ord a => Semigroup (BasicInterval a) where
  (<>) = intersect

instance Ord a => Monoid (BasicInterval a) where
  mempty = Full

oppositeInterval :: BasicInterval a -> Two (BasicInterval a)
oppositeInterval Full = None
oppositeInterval (Lower i) = One (Upper (_mirror i))
oppositeInterval (Upper i) = One (Lower (_mirror i))
oppositeInterval (BasicInterval l u) = Two (Upper (_mirror l)) (Lower (_mirror u))

pattern Lower, Upper :: BoundElement a -> BasicInterval a
pattern Lower x = BasicInterval x Infinity
pattern Upper x = BasicInterval Infinity x

pattern Full :: BasicInterval a
pattern Full = BasicInterval Infinity Infinity

_compare' :: Ord a => BoundElement a -> BoundElement a -> Ordering
_compare' Infinity Infinity = EQ
_compare' Infinity _ = LT
_compare' _ Infinity = GT
_compare' (Including v1) (Including v2) = compare v1 v2
_compare' (Including v1) (Excluding v2) = compare v1 v2 <> LT
_compare' (Excluding v1) (Including v2) = compare v1 v2 <> GT
_compare' (Excluding v1) (Excluding v2) = compare v1 v2

_compare'' :: Ord a => BasicInterval a -> BasicInterval a -> Ordering
_compare'' ~(BasicInterval l1 u1) ~(BasicInterval l2 u2) = _compare' l1 l2 <> _compare' u1 u2

_atBeg :: String -> ShowS -> ShowS
_atBeg = (.) . (++)

_atEnd :: String -> ShowS -> ShowS
_atEnd x f = (f "" ++) . (x ++)

_formatInterval :: Show a => String -> String -> String -> String -> BasicInterval a -> ShowS
_formatInterval sc so ec eo = form
  where
    form ~(BasicInterval lb ub) = incexc _atBeg sc so ('-' :) lb . (',' :) . incexc _atEnd ec eo id ub
    incexc f c o i = go
      where
        go Infinity = f o (i "∞" ++)
        go (Including v) = f c (showsPrec 10 v)
        go (Excluding v) = f o (showsPrec 10 v)

basicIntervalToStringS :: Show a => BasicInterval a -> ShowS
basicIntervalToStringS = _formatInterval "[" "(" "]" ")"

basicColouredIntervalToStringS :: Show a => BasicInterval a -> ShowS
basicColouredIntervalToStringS = _formatInterval (gr '[') (rd '(') (gr ']') (rd ')')
  where
    gr = ("\x1b[32m" ++) . (: "\x1b[0m")
    rd = ("\x1b[31m" ++) . (: "\x1b[0m")

-- mergeBasicInterval :: Ord a => BasicInterval a -> BasicInterval a -> Maybe (BasicInterval a)
-- mergeBasicInterval (BasicInterval l0

basicIntervalToString :: Show a => BasicInterval a -> String
basicIntervalToString = (`basicIntervalToStringS` "")

basicColouredIntervalToString :: Show a => BasicInterval a -> String
basicColouredIntervalToString = (`basicColouredIntervalToStringS` "")
