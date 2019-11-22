module Lexer
  ( Token(..)
  , safeTokenize
  , tokenize
  ) where

import           Data.Char (isAsciiLower, isDigit)

data Token
  = Plus
  | Minus
  | Equals
  | LeftParenthesis
  | RightParenthesis
  | Number Int
  | Variable Char
  deriving (Show, Eq)

safeTokenize :: String -> Either String [Token]
safeTokenize = validateVariables . tokenize

validateVariables :: [Token] -> Either String [Token]
validateVariables [] = return []
validateVariables (t@(Variable c):rest) =
  if isAsciiLower c
    then (t :) <$> validateVariables rest
    else Left $ "Illegal character: " ++ [c]
validateVariables (t:rest) = (t :) <$> validateVariables rest

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':s) = tokenize s
tokenize ('+':s) = Plus : tokenize s
tokenize ('-':s) = Minus : tokenize s
tokenize ('=':s) = Equals : tokenize s
tokenize ('(':s) = LeftParenthesis : tokenize s
tokenize (')':s) = RightParenthesis : tokenize s
tokenize s@(x:xs) =
  case parseDigits s of
    Just (number, rest) -> number : tokenize rest
    Nothing             -> Variable x : tokenize xs

parseDigits :: String -> Maybe (Token, String)
parseDigits s = do
  let (digits, rest) = span isDigit s
  if null digits
    then Nothing
    else Just (Number (read digits), rest)
