# Equation Parser Solver

Educational purpose implementation of parsing and solving an equation.

Steps:

 1. `Lexer.hs` translates `String` to `[Token]`.
 2. `Equationizer.hs` splits `[Token]` on `'='` into left-hand-side and right-hand-side.
 3. `TreeParser.hs` translates each sides `[Token]` into `Tree`.
 4. `Solver.hs` manipulates both sides of the equation until target variable is alone on one side.
 5. `Simplifier.hs` evaluates trees when they have no variables, otherwise return a new tree with sub expressions evaluated, so it contains as few integer literals as possibles.
