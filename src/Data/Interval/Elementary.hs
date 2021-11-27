module Data.Interval.Elementary where

data ElementaryBound a
  = Open a
  | Closed a
  | Infinity

data ElementaryInterval a = ElementaryInterval (ElementaryBound a) (ElementaryBound a)

leftCheck :: Ord a => ElementaryBound a -> a -> Bool
leftCheck (Open x) = (x <)
leftCheck (Closed x) = (x <=)
leftCheck Infinity = const True

rightCheck :: Ord a => ElementaryBound a -> a -> Bool
rightCheck (Open x) = (x >)
rightCheck (Closed x) = (x >=)
rightCheck Infinity = const True

containsItem :: Ord a => a -> ElementaryInterval a -> Bool
containsItem x (ElementaryInterval l r) = leftCheck l x && rightCheck r x

(∈) :: Ord a => a -> ElementaryInterval a -> Bool
(∈) = containsItem

(∋) :: Ord a => ElementaryInterval a -> a -> Bool
(∋) = flip containsItem

showInterval :: Show a => ElementaryInterval a -> String
showInterval = flip (showInterval' 0) ""

showInterval' :: Show a => Int -> ElementaryInterval a -> ShowS
showInterval' _ (ElementaryInterval ga gb) = fl ga . (',' :) . fr gb
  where fl (Open v) = ('(' :) . showsPrec 0 v
        fl (Closed v) = ('[' :) . showsPrec 0 v
        fl Infinity = ('(' : ) . ('−' : ) . ('∞' :)
        fr (Open v) = showsPrec 0 v .  (')' :)
        fr (Closed v) = showsPrec 0 v .  (']' :)
        fr Infinity = ('+' :) . ('∞' :) .  (')' :)
