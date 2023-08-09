{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Interval.Basic where

data BoundElement a
  = Including a
  | Excluding a
  | Infinity
  deriving (Eq, Functor, Read, Show)

_mirror :: BoundElement a -> BoundElement a
_mirror Infinity = Infinity
_mirror (Including x) = Excluding x
_mirror (Excluding x) = Including x

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
  {-# MINIMAL (isElement | isNotElement), (isEmpty, isNotEmpty) #-}

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

pattern Lower, Upper :: BoundElement a -> BasicInterval a
pattern Lower x = BasicInterval x Infinity
pattern Upper x = BasicInterval Infinity x

pattern Full :: BasicInterval a
pattern Full = BasicInterval Infinity Infinity

_compare' :: Ord a => BoundElement a -> BoundElement a -> Ordering
_compare' Infinity = go
  where go Infinity = EQ
        go _ = GT
_compare' (Including v) = go
  where go Infinity = LT
        go (Including v') = compare v v'
        go (Excluding _) = LT
_compare' (Excluding v) = go
  where go Infinity = LT
        go (Including _) = GT
        go (Excluding v') = compare v' v

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

excluding :: BasicInterval a -> BasicInterval a -> BasicInterval a
excluding = undefined

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
