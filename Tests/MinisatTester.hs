-- Some tests for Minisat

import Minisat.Minisat
import Model.Model

main :: IO ()
main =
  do
  -- 0 && not(0)
  solver <- newSolver
  addVars solver 2
  addClauses solver [[Var 0], [Neg 0]]
  res <- solve solver
  deleteSolver solver
  if res then error "0 && not(0) : SAT" else return ()
  -- (0 || 1) && (0 || not(1))
  solver <- newSolver
  addVars solver 4
  addClauses solver [[Var 0, Var 1], [Var 0, Neg 1]]
  res <- solve solver
  if (not res)
    then error "(0 || 1) && (0 || not(1)) : UNSAT"
    else return ()
  deleteSolver solver
  -- 0 && (2 || 3) && not(3)
  solver <- newSolver
  addVars solver 7
  addClauses solver [[Var 0], [Var 2, Var 3], [Neg 3]]
  res <- solve solver
  if (not res)
    then error "0 && (2 || 3) && not(3) : UNSAT"
    else return ()
  true <- getVarValue solver 2
  if (true == 0)
    then error "0 && (2 || 3) && not(3) : SAT with Neg 2"
    else return ()
  deleteSolver solver
  -- Solve with assumptions: not(0), assuming 0
  solver <- newSolver
  addVars solver 2
  addClauses solver [[Neg 0], [Neg 1]]
  res <- solveWithAssumps solver [Var 0, Var 1]
  if res then error "0 && 1 => not(0) && not(1) : SAT" else return ()
  print "Done."
  
