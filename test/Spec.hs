import           Test.HUnit

import qualified Equationizer as E
import qualified Lexer        as L
import qualified Simplifier   as SS
import qualified Solver       as S
import qualified TreeParser   as T

testLexer =
  [ L.safeTokenize "1+1 = x-(2 - 1)" ~?=
    Right
      [ L.Number 1
      , L.Plus
      , L.Number 1
      , L.Equals
      , L.Variable 'x'
      , L.Minus
      , L.LeftParenthesis
      , L.Number 2
      , L.Minus
      , L.Number 1
      , L.RightParenthesis
      ]
  , L.safeTokenize "1 + $" ~?= Left "Illegal character: $"
  , L.safeTokenize "-5+( -x )" ~?=
    Right [L.Minus, L.Number 5, L.Plus, L.LeftParenthesis, L.Minus, L.Variable 'x', L.RightParenthesis]
  ]

testEquationizer =
  [ E.equationize "1 = 2" ~?= Right (E.Equation (T.Number 1) (T.Number 2))
  , E.equationize "+ = 1" ~?= Left "LHS parse error"
  , E.equationize "1 = +" ~?= Left "RHS parse error"
  , case E.equationize "shit" of
      Left _   -> TestCase $ return ()
      Right eq -> TestCase $ assertString ("Expected fail, got: " ++ show eq)
  ]

testTreeParser =
  [ T.parseString "5 - (x - 1)" ~?= Right (T.Minus (T.Number 5) (T.Minus (T.Variable 'x') (T.Number 1)), [])
  , T.parseString "-3" ~?= Right (T.Negate (T.Number 3), [])
  , T.parseString "1+(-x)" ~?= Right (T.Plus (T.Number 1) (T.Negate (T.Variable 'x')), [])
  , T.parseString "-(-x)" ~?= Right (T.Negate (T.Negate (T.Variable 'x')), [])
  , T.parseString "-1+2" ~?= Right (T.Plus (T.Negate (T.Number 1)) (T.Number 2), [])
  ]

testSolver =
  [ assertSolveEquation 'x' "x+1=5" "5-1"
  , assertSolveEquation 'x' "1+x=5" "5-1"
  , assertSolveEquation 'x' "x-1=5" "5+1"
  , assertSolveEquation 'x' "1-x=5" "1-5"
  , assertSolveEquation 'x' "1+(x-3)=10+15" "10+15-1+3"
  , assertSolveEquation 'z' "x=y+z" "x-y"
  ]

testSolverFail =
  [ assertSolveFail 'x' "10+x=10-x" "Variable in both sides of equation, not yet supported"
  , assertSolveFail 'x' "y=1+1" "Variable not in equation"
  ]

testSimplifier =
  [ assertSimplify "1+1+1" "3"
  , assertSimplify "1+1+x" "2+x"
  , assertSimplify "x+1+1" "x+2"
  , assertSimplify "x+0" "x"
  , assertSimplify "0+0" "0"
  , assertSimplify "1+1" "2"
  , assertSimplify "-x+y" "y-x"
  , assertSimplify "-5+7" "2"
  , assertSimplify "10-(3-x)" "7+x"
  , assertSimplify "10-(x-3)" "13-x"
  , assertSimplify "20+(3-x)" "23-x"
  , assertSimplify "20+(x-3)" "17+x"
  , assertSimplify "100-(3+x)" "97-x"
  , assertSimplify "100-(x+3)" "97-x"
  , assertSimplify "200+(3+x)" "203+x"
  , assertSimplify "200+(x+3)" "203+x"
  , assertSimplify "-(-(1+1))" "2"
  , assertSimplify "2+(2-2)+(1+1+(5-(1+x)))" "8-x"
  -- , assertSimplify "x-1+y-1" "x+y-2" -- multi variable not fully supported
  ]

assertSolveEquation :: Char -> String -> String -> Test
assertSolveEquation var equation expected =
  let info = "Input " ++ show (var, equation, expected)
   in case parse' of
        Left err         -> TestCase . assertString $ info ++ ": " ++ err
        Right (eq, tree) -> info ~: S.solveFor var eq ~?= Right tree
  where
    parse' :: Either String (E.Equation, T.Tree)
    parse' = do
      equation' <- E.equationize equation
      (expected', _) <- T.parseString expected
      return (equation', expected')

assertSolveFail :: Char -> String -> String -> Test
assertSolveFail var equation expectedFail =
  let info = "Input " ++ show (var, equation)
   in case E.equationize equation of
        Left err -> TestCase . assertString $ info ++ ": " ++ err
        Right eq -> info ~: S.solveFor var eq ~?= Left expectedFail

assertSimplify :: String -> String -> Test
assertSimplify input expected =
  case parseAndTest of
    Left err -> TestCase . assertString $ "Failed " ++ show (input, expected) ++ ": " ++ err
    Right test -> test
  where
    parseAndTest = do
      (beforeSimplify, _) <- T.parseString input
      (expectedTree, _) <- T.parseString expected
      let simplified = SS.simplify beforeSimplify
      return $ simplified ~?= expectedTree

testList = TestList $ testLexer ++ testEquationizer ++ testTreeParser ++ testSolver ++ testSolverFail ++ testSimplifier

-- https://caiorss.github.io/Functional-Programming/haskell/UnitTest_Hunit.html
main :: IO ()
main = do
  runTestTT testList
  return ()
