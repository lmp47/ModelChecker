-- Some tests for Minisat

import Minisat.Minisat
import Model.Model



main :: IO ()
main =
  do
  -- 0 && not(0)
  print $ show $ satisfiable $ solve $ addClauses (addVars newSolver 2) [[Var 0], [Neg 0]]
  -- (0 || 1) && (0 || not(1))
  print $ show $ satisfiable $ solve $ addClauses (addVars newSolver 4) [[Var 0, Var 1], [Var 0, Neg 1]]
  -- 0 && (2 || 3) && not(3)
  print $ show $ satisfiable $ solve $ addClauses (addVars newSolver 7) [[Var 0], [Var 2, Var 3], [Neg 3]]
  -- Solve with assumptions: not(0) && not(1), assuming 0 && 1
  print $ show $ satisfiable $ solveWithAssumps (addClauses (addVars newSolver 4) [[Neg 0], [Neg 1]]) [Var 0, Var 1]
  -- Solve with assumptions: (not(0) || 0) && not(1), assuming not(0)
  print $ show $ satisfiable $ solveWithAssumps (addClauses (addVars newSolver 4) [[Neg 0, Var 0], [Neg 1]]) [Neg 0]
  let solver = (addClauses (addVars newSolver 4) [[Var 0, Var 1], [Neg 1]])
  print $ show $ satisfiable $ solve $ solver
  print $ show $  getLiterals solver []
  let solver' = addVars newSolver 2
  print $ show $ satisfiable $ solveWithAssumps solver [Neg 1]
  print $ show $ getLiterals solver []
  --print $ show $ getLiterals solver
  print $ show $ conflict $ solveWithAssumps solver' [Neg 0, Var 0]
  let solver'' = addClauses (addVars newSolver 18) [[Var' 0,Var 0],[Neg' 0,Neg 0],[Var' 1,Var 1],[Neg' 1,Neg 1],[Var' 2,Var 8],[Neg' 2,Neg 8],[Neg' 8,Neg' 7],[Neg' 8,Neg' 6],[Var' 7,Var' 6,Var' 8],[Neg' 7,Neg' 5],[Neg' 7,Neg' 0],[Var' 5,Var' 0,Var' 7],[Neg' 6,Var' 5],[Neg' 6,Var' 0],[Neg' 5,Neg' 0,Var' 6],[Neg' 5,Neg' 4],[Neg' 5,Neg' 3],[Var' 4,Var' 3,Var' 5],[Neg' 4,Var' 2],[Neg' 4,Neg' 1],[Neg' 2,Var' 1,Var' 4],[Neg' 3,Neg' 2],[Neg' 3,Var' 1],[Var' 2,Neg' 1,Var' 3]]
  --let solver''' = addClauses solver'' [[Var' 4, Var' 2, Var' 8], [Var 8],[Neg 5,Neg 4],[Neg 6,Var 0],[Neg 7,Neg 5],[Neg 2],
  --                  [Neg 5,Neg 3],[Var 4,Var 3,Var 5],[Neg 4,Var 2],[Neg 4,Neg 1],[Neg 2,Var 1,Var 4],[Neg 3,Neg 2],
  --                  [Neg 3,Var 1], [Var 2,Neg 1,Var 3] ]
  let solver''' = addClauses solver'' [[Neg 7],[Neg 6],[Neg 4],[Var 8],[Neg 2]]
  print $ show $ getLiterals solver''' []
  print $ show $ satisfiable $ solveWithAssumps solver''' [Var' 7]
  let issueSolver = addClauses (addVars newSolver 90) transitionC
  print "Issue, no assumps: "
  print $ show $ satisfiable $ solve issueSolver
  print "Issue, assume Neg 22 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Var' 44]
  print "Issue, assume Neg 22 and Neg 25 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Neg 25, Var' 44]
  print "Issue, assume Neg 22 and Neg 25 and Neg 20 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Neg 25, Neg 20, Var' 44]
  print $ show $ conflict $ solveWithAssumps issueSolver [Neg 22, Neg 25, Neg 20, Var' 44]
  print "Issue, assume Neg 22 and Neg 25 and Var 20 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Neg 25, Var 20, Var' 44]
  print $ show $ conflict $ solveWithAssumps issueSolver [Neg 22, Neg 25, Var 20, Var' 44]
  print "Issue, assume Neg 22 and Var 25 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Var 25, Var' 44]
  print "Issue, assume Neg 22 and Var 25 and Neg 20 and Var' 44: "
  print $ show $ satisfiable $ solveWithAssumps issueSolver [Neg 22, Var 25, Neg 20, Var' 44]
  print $ show $ conflict $ solveWithAssumps issueSolver [Neg 22, Var 25, Neg 20, Var' 44]
  print "Done."

transitionC :: [Clause]
transitionC = [[Var' 0,Var 0],[Neg' 0,Neg 0],[Var' 1,Var 8],[Neg' 1,Neg 8],[Var' 2,Var 15],[Neg' 2,Neg 15],[Var' 3,Var 3],[Neg' 3,Neg 3],[Var' 4,Var 21],[Neg' 4,Neg 21],[Var' 5,Neg 43],[Neg' 5,Var 43],[Neg' 44,Var' 32],[Neg' 44,Var' 2],[Neg' 32,Neg' 2,Var' 44],[Neg' 43,Neg' 42],[Neg' 43,Var' 41],[Var' 42,Neg' 41,Var' 43],[Neg' 42,Var' 28],[Neg' 42,Var' 2],[Neg' 28,Neg' 2,Var' 42],[Neg' 41,Neg' 40],[Neg' 41,Var' 38],[Var' 40,Neg' 38,Var' 41],[Neg' 40,Var' 39],[Neg' 40,Var' 2],[Neg' 39,Neg' 2,Var' 40],[Neg' 39,Var' 5],[Neg' 39,Var' 3],[Neg' 5,Neg' 3,Var' 39],[Neg' 38,Neg' 37],[Neg' 38,Var' 34],[Var' 37,Neg' 34,Var' 38],[Neg' 37,Var' 36],[Neg' 37,Var' 2],[Neg' 36,Neg' 2,Var' 37],[Neg' 36,Var' 35],[Neg' 36,Neg' 3],[Neg' 35,Var' 3,Var' 36],[Neg' 35,Neg' 5],[Neg' 35,Neg' 4],[Var' 5,Var' 4,Var' 35],[Neg' 34,Neg' 33],[Neg' 34,Var' 31],[Var' 33,Neg' 31,Var' 34],[Neg' 33,Var' 32],[Neg' 33,Neg' 2],[Neg' 32,Var' 2,Var' 33],[Neg' 32,Var' 28],[Neg' 32,Var' 3],[Neg' 28,Neg' 3,Var' 32],[Neg' 31,Neg' 30],[Neg' 31,Var' 27],[Var' 30,Neg' 27,Var' 31],[Neg' 30,Var' 29],[Neg' 30,Neg' 2],[Neg' 29,Var' 2,Var' 30],[Neg' 29,Var' 28],[Neg' 29,Neg' 3],[Neg' 28,Var' 3,Var' 29],[Neg' 28,Var' 5],[Neg' 28,Var' 4],[Neg' 5,Neg' 4,Var' 28],[Neg' 27,Neg' 26],[Neg' 27,Neg' 24],[Var' 26,Var' 24,Var' 27],[Neg' 26,Var' 25],[Neg' 26,Neg' 2],[Neg' 25,Var' 2,Var' 26],[Neg' 25,Var' 5],[Neg' 25,Neg' 4],[Neg' 5,Var' 4,Var' 25],[Neg' 24,Var' 23],[Neg' 24,Neg' 2],[Neg' 23,Var' 2,Var' 24],[Neg' 23,Var' 22],[Neg' 23,Var' 3],[Neg' 22,Neg' 3,Var' 23],[Neg' 22,Neg' 5],[Neg' 22,Var' 4],[Var' 5,Neg' 4,Var' 22],[Neg' 21,Neg' 20],[Neg' 21,Neg' 19],[Var' 20,Var' 19,Var' 21],[Neg' 20,Neg' 18],[Neg' 20,Neg' 2],[Var' 18,Var' 2,Var' 20],[Neg' 19,Var' 18],[Neg' 19,Var' 2],[Neg' 18,Neg' 2,Var' 19],[Neg' 18,Neg' 17],[Neg' 18,Neg' 16],[Var' 17,Var' 16,Var' 18],[Neg' 17,Var' 4],[Neg' 17,Neg' 3],[Neg' 4,Var' 3,Var' 17],[Neg' 16,Neg' 4],[Neg' 16,Var' 3],[Var' 4,Neg' 3,Var' 16],[Neg' 15,Neg' 14],[Neg' 15,Var' 12],[Var' 14,Neg' 12,Var' 15],[Neg' 14,Var' 13],[Neg' 14,Neg' 0],[Neg' 13,Var' 0,Var' 14],[Neg' 13,Var' 2],[Neg' 13,Var' 1],[Neg' 2,Neg' 1,Var' 13],[Neg' 12,Neg' 11],[Neg' 12,Neg' 10],[Var' 11,Var' 10,Var' 12],[Neg' 11,Var' 2],[Neg' 11,Neg' 1],[Neg' 2,Var' 1,Var' 11],[Neg' 10,Var' 9],[Neg' 10,Var' 0],[Neg' 9,Neg' 0,Var' 10],[Neg' 9,Neg' 2],[Neg' 9,Var' 1],[Var' 2,Neg' 1,Var' 9],[Neg' 8,Neg' 7],[Neg' 8,Neg' 6],[Var' 7,Var' 6,Var' 8],[Neg' 7,Var' 1],[Neg' 7,Neg' 0],[Neg' 1,Var' 0,Var' 7],[Neg' 6,Neg' 1],[Neg' 6,Var' 0],[Var' 1,Neg' 0,Var' 6],[Neg 44,Var 32],[Neg 44,Var 2],[Neg 32,Neg 2,Var 44],[Neg 43,Neg 42],[Neg 43,Var 41],[Var 42,Neg 41,Var 43],[Neg 42,Var 28],[Neg 42,Var 2],[Neg 28,Neg 2,Var 42],[Neg 41,Neg 40],[Neg 41,Var 38],[Var 40,Neg 38,Var 41],[Neg 40,Var 39],[Neg 40,Var 2],[Neg 39,Neg 2,Var 40],[Neg 39,Var 5],[Neg 39,Var 3],[Neg 5,Neg 3,Var 39],[Neg 38,Neg 37],[Neg 38,Var 34],[Var 37,Neg 34,Var 38],[Neg 37,Var 36],[Neg 37,Var 2],[Neg 36,Neg 2,Var 37],[Neg 36,Var 35],[Neg 36,Neg 3],[Neg 35,Var 3,Var 36],[Neg 35,Neg 5],[Neg 35,Neg 4],[Var 5,Var 4,Var 35],[Neg 34,Neg 33],[Neg 34,Var 31],[Var 33,Neg 31,Var 34],[Neg 33,Var 32],[Neg 33,Neg 2],[Neg 32,Var 2,Var 33],[Neg 32,Var 28],[Neg 32,Var 3],[Neg 28,Neg 3,Var 32],[Neg 31,Neg 30],[Neg 31,Var 27],[Var 30,Neg 27,Var 31],[Neg 30,Var 29],[Neg 30,Neg 2],[Neg 29,Var 2,Var 30],[Neg 29,Var 28],[Neg 29,Neg 3],[Neg 28,Var 3,Var 29],[Neg 28,Var 5],[Neg 28,Var 4],[Neg 5,Neg 4,Var 28],[Neg 27,Neg 26],[Neg 27,Neg 24],[Var 26,Var 24,Var 27],[Neg 26,Var 25],[Neg 26,Neg 2],[Neg 25,Var 2,Var 26],[Neg 25,Var 5],[Neg 25,Neg 4],[Neg 5,Var 4,Var 25],[Neg 24,Var 23],[Neg 24,Neg 2],[Neg 23,Var 2,Var 24],[Neg 23,Var 22],[Neg 23,Var 3],[Neg 22,Neg 3,Var 23],[Neg 22,Neg 5],[Neg 22,Var 4],[Var 5,Neg 4,Var 22],[Neg 21,Neg 20],[Neg 21,Neg 19],[Var 20,Var 19,Var 21],[Neg 20,Neg 18],[Neg 20,Neg 2],[Var 18,Var 2,Var 20],[Neg 19,Var 18],[Neg 19,Var 2],[Neg 18,Neg 2,Var 19],[Neg 18,Neg 17],[Neg 18,Neg 16],[Var 17,Var 16,Var 18],[Neg 17,Var 4],[Neg 17,Neg 3],[Neg 4,Var 3,Var 17],[Neg 16,Neg 4],[Neg 16,Var 3],[Var 4,Neg 3,Var 16],[Neg 15,Neg 14],[Neg 15,Var 12],[Var 14,Neg 12,Var 15],[Neg 14,Var 13],[Neg 14,Neg 0],[Neg 13,Var 0,Var 14],[Neg 13,Var 2],[Neg 13,Var 1],[Neg 2,Neg 1,Var 13],[Neg 12,Neg 11],[Neg 12,Neg 10],[Var 11,Var 10,Var 12],[Neg 11,Var 2],[Neg 11,Neg 1],[Neg 2,Var 1,Var 11],[Neg 10,Var 9],[Neg 10,Var 0],[Neg 9,Neg 0,Var 10],[Neg 9,Neg 2],[Neg 9,Var 1],[Var 2,Neg 1,Var 9],[Neg 8,Neg 7],[Neg 8,Neg 6],[Var 7,Var 6,Var 8],[Neg 7,Var 1],[Neg 7,Neg 0],[Neg 1,Var 0,Var 7],[Neg 6,Neg 1],[Neg 6,Var 0],[Var 1,Neg 0,Var 6]]
