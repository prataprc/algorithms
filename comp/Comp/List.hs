module Comp.List
( minusS, unionS, foldi, foldt )
where


-- | Union of two sorted list.
unionS (x:xs) (y:ys)
  | x <  y    = x:unionS xs     (y:ys)
  | x == y    = x:unionS xs     ys 
  | otherwise = y:unionS (x:xs) ys
unionS xs     []    = xs
unionS []     ys    = ys


-- | Remove `ys` elements from `xs` list and return the remaining list.
--   Both `ys` and `xs` list are expected to be sorted.
minusS (x:xs) (y:ys)
  | x <  y    = x:minusS xs     (y:ys)
  | x == y    =   minusS xs     ys 
  | otherwise =   minusS (x:xs) ys
minusS xs _    = xs


-- | Tree like folds over a list. Applicable for associative `f` application.
--   Refer : http://www.haskell.org/haskellwiki/Fold
foldt :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
-- Lazy version
foldi :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
-- Associative pairs
pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t
