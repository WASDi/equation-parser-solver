module Equationizer
  ( equationize
  , Equation(..)
  ) where

import           Debug.Trace
import           Lexer       (Token (..), safeTokenize)
import           TreeParser

data Equation =
  Equation
    { lhs :: Tree
    , rhs :: Tree
    }
  deriving (Show, Eq)

equationize :: String -> Either String Equation
equationize s = do
  tokens <- safeTokenize s
  (lhs, rhs) <- maybeToRight "Not a proper equation, '=' must appear in middle." $ equalsSplit tokens
  (lhs', lhsRest) <- maybeToRight "LHS parse error" $ parse lhs
  (rhs', rhsRest) <- maybeToRight "RHS parse error" $ parse rhs
  ensure (null lhsRest) $ "Remaining tokens after LHS parse: " ++ show lhsRest
  ensure (null rhsRest) $ "Remaining tokens after RHS parse: " ++ show rhsRest
  return $ Equation lhs' rhs'

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight x Nothing  = Left x
maybeToRight _ (Just x) = Right x

ensure :: Bool -> String -> Either String ()
ensure False error = Left error
ensure True _      = Right ()

equalsSplit :: [Token] -> Maybe ([Token], [Token])
equalsSplit = go False [] [] . reverse
  where
    go :: Bool -> [Token] -> [Token] -> [Token] -> Maybe ([Token], [Token])
    go _ lhs rhs [] =
      if null lhs || null rhs
        then Nothing -- end of input with empty sides
        else Just (lhs, rhs)
    go eqFound lhs rhs (Equals:ts) =
      if eqFound
        then Nothing -- multiple equals
        else go True lhs rhs ts
    go eqFound lhs rhs (t:ts) =
      if eqFound
        then go eqFound (t : lhs) rhs ts
        else go eqFound lhs (t : rhs) ts
