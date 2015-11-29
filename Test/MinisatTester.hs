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
  let solver = (addClauses (addVars newSolver 4) [[Var 0, Var 1], [Neg 1]])
  print $ show $ solve $ solver
  print $ show $ getLiterals solver [] 4
  let solver' = addVars newSolver 2
  print $ show $ solveWithAssumps solver [Neg 1]
  print $ show $ getLiterals solver [] 4
  --print $ show $ getLiterals solver 4
  print $ getConflictWithAssumps solver' [Neg 0, Var 0]
  let solver'' = addClauses (addVars newSolver 18) [[Var' 0,Var 0],[Neg' 0,Neg 0],[Var' 1,Var 1],[Neg' 1,Neg 1],[Var' 2,Var 8],[Neg' 2,Neg 8],[Neg' 8,Neg' 7],[Neg' 8,Neg' 6],[Var' 7,Var' 6,Var' 8],[Neg' 7,Neg' 5],[Neg' 7,Neg' 0],[Var' 5,Var' 0,Var' 7],[Neg' 6,Var' 5],[Neg' 6,Var' 0],[Neg' 5,Neg' 0,Var' 6],[Neg' 5,Neg' 4],[Neg' 5,Neg' 3],[Var' 4,Var' 3,Var' 5],[Neg' 4,Var' 2],[Neg' 4,Neg' 1],[Neg' 2,Var' 1,Var' 4],[Neg' 3,Neg' 2],[Neg' 3,Var' 1],[Var' 2,Neg' 1,Var' 3]]
  --let solver''' = addClauses solver'' [[Var' 4, Var' 2, Var' 8], [Var 8],[Neg 5,Neg 4],[Neg 6,Var 0],[Neg 7,Neg 5],[Neg 2],
  --                  [Neg 5,Neg 3],[Var 4,Var 3,Var 5],[Neg 4,Var 2],[Neg 4,Neg 1],[Neg 2,Var 1,Var 4],[Neg 3,Neg 2],
  --                  [Neg 3,Var 1], [Var 2,Neg 1,Var 3] ]
  let solver''' = addClauses solver'' [[Neg 7],[Neg 6],[Neg 4],[Var 8],[Neg 2]]
  print $ show $ getLiterals solver''' [] 18
  print $ show $ solveWithAssumps solver''' [Var' 7]
  print "Done."
