{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Interval.Basic where

data Two a
  = None
  | One a
  | Two a
  deriving (Eq, Functor, Read, Show)

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

pattern Lower, Upper :: BoundElement a -> BasicInterval a
pattern Lower x = BasicInterval x Infinity
pattern Upper x = BasicInterval Infinity x

pattern Full :: BasicInterval a
pattern Full = BasicInterval Infinity Infinity

_compare' :: Ord a => BoundElement a -> BoundElement a -> Ordering
_compare' Infinity = go
  where go Infinity = EQ
        go _ = LT
_compare' (Including v) = go
  where go Infinity = GT
        go (Including v') = compare v v'
        go (Excluding _) = LT
_compare' (Excluding v) = go
  where go (Excluding v') = compare v' v
        go _ = GT

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
