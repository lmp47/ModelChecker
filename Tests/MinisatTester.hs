-- Some tests for Minisat

import Minisat.Minisat
import Model.Model

main :: IO ()
main =
  do
  -- 0 && not(0)
  print $ show $ solve $ addClauses (addVars newSolver 2) [[Var 0], [Neg 0]]
  -- (0 || 1) && (0 || not(1))
  print $ show $ solve $ addClauses (addVars newSolver 4) [[Var 0, Var 1], [Var 0, Neg 1]]
  -- 0 && (2 || 3) && not(3)
  print $ show $ solve $ addClauses (addVars newSolver 7) [[Var 0], [Var 2, Var 3], [Neg 3]]
  -- Solve with assumptions: not(0) && not(1), assuming 0 && 1
  print $ show $ solveWithAssumps (addClauses (addVars newSolver 4) [[Neg 0], [Neg 1]]) [Var 0, Var 1]
  -- Solve with assumptions: (not(0) || 0) && not(1), assuming not(0)
  print $ show $ solveWithAssumps (addClauses (addVars newSolver 4) [[Neg 0, Var 0], [Neg 1]]) [Neg 0]
  print "Done."
