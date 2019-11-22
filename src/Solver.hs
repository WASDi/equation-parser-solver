module Solver
  ( solveFor
  ) where

import           Equationizer
import           TreeParser

type Var = Char

data Result
  = Solved Tree
  | Proceed Equation
  | Failed String

solveFor :: Var -> Equation -> Either String Tree
solveFor var equation =
  case solveOneStep var equation of
    Solved tree       -> Right tree
    Proceed equation' -> solveFor var equation'
    Failed s          -> Left s

hasVariable :: Tree -> Var -> Bool
hasVariable tree var = hasVariable' tree
  where
    hasVariable' (Plus t1 t2)    = hasVariable' t1 || hasVariable' t2
    hasVariable' (Minus t1 t2)   = hasVariable' t1 || hasVariable' t2
    hasVariable' (Number _)      = False
    hasVariable' (Variable var') = var == var'

isSolvedFor :: Var -> Tree -> Bool
isSolvedFor var (Variable var') = var == var'
isSolvedFor _ _                 = False

solveOneStep :: Var -> Equation -> Result
solveOneStep var (Equation lhs rhs) =
  case extract of
    Left error                 -> Failed error
    Right (varSide, otherSide) -> solveOneStep' var varSide otherSide
  where
    extract :: Either String (Tree, Tree)
    extract = do
      let varInLHS = hasVariable lhs var
      let varInRHS = hasVariable rhs var
      failIf (varInLHS && varInRHS) "Variable in both sides of equation, not yet supported"
      failIf (not varInLHS && not varInRHS) "Variable not in equation"
      return $
        if varInLHS
          then (lhs, rhs)
          else (rhs, lhs)

solveOneStep' :: Var -> Tree -> Tree -> Result
solveOneStep' var varSide otherSide =
  case varSide of
    Variable _ -> Solved otherSide
    Plus t1 t2 ->
      if hasVariable t1 var
        then Proceed $ Equation t1 (Minus otherSide t2)
        else Proceed $ Equation t2 (Minus otherSide t1)
    Minus t1 t2 -> Proceed $ Equation t1 (Plus otherSide t2)

failIf :: Bool -> String -> Either String ()
failIf True s  = Left s
failIf False _ = Right ()
