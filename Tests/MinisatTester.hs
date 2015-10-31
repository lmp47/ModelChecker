-- Some tests for Minisat

import Minisat.Minisat
import Parser.AigModel(Lit(Var, Neg))

main :: IO ()
main =
  do
  -- 0 && not(0)
  solver <- newSolver
  addVars solver 1
  addClauses solver [[Var 0], [Neg 0]]
  res <- solve solver
  deleteSolver solver
  if res then error "0 && not(0) : SAT" else return ()
  -- (0 || 1) && (0 || not(1))
  solver <- newSolver
  addVars solver 2
  addClauses solver [[Var 0, Var 1], [Var 0, Neg 1]]
  res <- solve solver
  if (not res)
    then error "(0 || 1) && (0 || not(1)) : UNSAT"
    else return ()
  deleteSolver solver
  -- 0 && (2 || 3) && not(3)
  solver <- newSolver
  addVars solver 4
  addClauses solver [[Var 0], [Var 2, Var 3], [Neg 3]]
  res <- solve solver
  if (not res)
    then error "0 && (2 || 3) && not(3) : UNSAT"
    else return ()
  true <- getVarValue solver 2
  if (not $ true == 1)
    then error "0 && (2 || 3) && not(3) : SAT with Neg 2"
    else return ()
  deleteSolver solver
  print "Done."
  
