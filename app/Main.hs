module Main where

import           Control.Monad (forever)
import qualified Equationizer  as E
import qualified Solver        as S
import qualified Simplifier        as SS
import qualified TreeParser    as T

main :: IO ()
main =
  forever $ do
    putStrLn "> "
    input <- getLine
    case solveX input of
      Left error -> putStrLn $ "ERROR !!! " ++ error
      Right nice -> putStrLn nice

solveX :: String -> Either String String
solveX input = do
  equation <- E.equationize input
  tree <- S.solveFor 'x' equation
  return . show $ SS.simplify tree
