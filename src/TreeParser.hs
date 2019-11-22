module TreeParser
  ( parse
  , parseString
  , Tree(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad       ((>=>))
import qualified Lexer               as T

data Tree
  = Plus Tree Tree
  | Minus Tree Tree
  | Negate Tree
  | Number Int
  | Variable Char
  deriving (Show, Eq)

newtype Parser a =
  Parser
    { runParser :: [T.Token] -> Maybe (a, [T.Token])
    }

instance Functor Parser where
  fmap f (Parser parse) = Parser (parse >=> (\(x, rest) -> Just (f x, rest)))

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser p1) <*> (Parser p2) =
    Parser $ \s -> do
      (f, rest1) <- p1 s
      (x, rest2) <- p2 rest1
      return (f x, rest2)

instance Monad Parser where
  return = pure
  (Parser parse) >>= f =
    Parser $ \s -> do
      (x, rest) <- parse s
      let (Parser parseB) = f x
      parseB rest

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) =
    Parser $ \s ->
      case p1 s of
        Just x  -> Just x
        Nothing -> p2 s

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain parse combiner = do
  checkpoint <- parse
  parseMore checkpoint
  where
    parseMore checkpoint =
      (do f <- combiner
          more <- parse
          parseMore (f checkpoint more)) <|>
      return checkpoint

-- http://dev.stephendiehl.com/fun/002_parsers.html
parseString :: String -> Either String (Tree, [T.Token])
parseString s =
  case T.safeTokenize s of
    Left s -> Left s
    Right tokens ->
      case parse tokens of
        Nothing -> Left "parse error !!!"
        Just x  -> Right x

parse :: [T.Token] -> Maybe (Tree, [T.Token])
parse = runParser parseTree

parseTree :: Parser Tree
parseTree = parseTerm `chain` plusOrMinus
  where
    plusOrMinus = (skip T.Plus >> return Plus) <|> (skip T.Minus >> return Minus)

parseTerm :: Parser Tree
parseTerm = parseNumber <|> parseVariable <|> parseParenthesis <|> parseNegation

parseParenthesis :: Parser Tree
parseParenthesis = do
  skip T.LeftParenthesis
  inside <- parseTree
  skip T.RightParenthesis
  return inside

parseNegation :: Parser Tree
parseNegation = do
  skip T.Minus
  Negate <$> parseTerm

parseNumber :: Parser Tree
parseNumber = Parser go
  where
    go :: [T.Token] -> Maybe (Tree, [T.Token])
    go (T.Number n:rest) = Just (Number n, rest)
    go _                 = Nothing

parseVariable :: Parser Tree
parseVariable = Parser go
  where
    go :: [T.Token] -> Maybe (Tree, [T.Token])
    go (T.Variable c:rest) = Just (Variable c, rest)
    go _                   = Nothing

skip :: T.Token -> Parser ()
skip toSkip = Parser skip'
  where
    skip' [] = Nothing
    skip' (t:ts) =
      if toSkip == t
        then Just ((), ts)
        else Nothing
