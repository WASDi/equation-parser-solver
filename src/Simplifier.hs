module Simplifier
  ( simplify
  ) where

import           TreeParser

-- If the input contains variables, return a minimized tree where integer sub expressions have been evaluated. Otherwise evaluate
simplifyEvaluate :: Tree -> Either Tree Int
simplifyEvaluate tree =
  let simplified = simplify tree
   in case evaluateLeafTree simplified of
        Just evaluated -> Right evaluated
        Nothing        -> Left tree

simplify :: Tree -> Tree
simplify (Plus t1 t2) =
  case simplify' Plus (+) t1 t2 of
    (Plus t (Number n))
      | n == 0 -> t
      | n < 0 -> Minus t (Number (abs n))
    (Plus (Number n) t)
      | n == 0 -> t
      | n < 0 -> Minus t (Number (abs n))
    (Plus t1 (Negate t2)) -> simplify $ Minus t1 t2
    (Plus (Negate t1) t2) -> simplify $ Minus t2 t1
    t -> t
simplify (Minus t1 t2) =
  case simplify' Minus (-) t1 t2 of
    (Minus t (Number n))
      | n == 0 -> t
      | n < 0 -> Plus t (Number (abs n))
    (Minus (Number 0) t) -> Negate t
    (Minus t (Negate n)) -> Plus t n
    t -> t
simplify (Negate (Negate t)) = simplify t
simplify leaf = leaf

simplify' :: (Tree -> Tree -> Tree) -> (Int -> Int -> Int) -> Tree -> Tree -> Tree
simplify' cons op' t1 t2 =
  let op n1 n2 = Number (n1 `op'` n2)
   in case (simplify t1, simplify t2) of
        (Number n1, Number n2)           -> n1 `op` n2
        (Plus t (Number n1), Number n2)  -> t `Plus` (n1 `op` n2)
        (Plus (Number n1) t, Number n2)  -> t `Plus` (n1 `op` n2)
        (Minus t (Number n1), Number n2) -> t `Minus` (n1 `op` n2)
        (Minus (Number n1) t, Number n2) -> t `Minus` (n1 `op` n2)
        (Number n1, Plus (Number n2) t)  -> (n1 `op` n2) `cons` t
        (Number n1, Plus t (Number n2))  -> (n1 `op` n2) `cons` t
        (Number n1, Minus (Number n2) t) -> (n1 `op` n2) `cons` Negate t
        (Number n1, Minus t (Number n2)) -> (n1 `op` negate n2) `cons` t
        (t1', t2')                       -> cons t1' t2'

evaluateLeafTree :: Tree -> Maybe Int
evaluateLeafTree (Number x) = Just x
evaluateLeafTree _          = Nothing
