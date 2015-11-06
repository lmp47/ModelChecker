{-|
Module: Minisat
Description: Haskell bindings for some Minisat functions

Haskell bindings for some Minisat functions.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Minisat.Minisat ( Solver
                       , newSolver
                       , deleteSolver
                       , addVars
                       , printVars
                       , getVarValue
                       , addClause
                       , addClauses
                       , solve
                       , solveWithAssumps ) where

import Model.Model
import Foreign
import Foreign.C.Types
import Foreign.C.String

-----------------------
-- Wrapper Functions --
-----------------------

-- | Adds variables 0 to n-1 to the solver.
addVars :: Solver -> Word -> IO ()
addVars solver n =
  if n > 0 then do
    newMinisatVar solver (fromIntegral $ n - 1) 0
    addVars solver (n - 1)
  else return ()

-- | Prints variables 0 to n-1 in the solver.
printVars :: Solver -> Word -> IO ()
printVars solver n =
  let k = fromIntegral $ n - 1 in
  do
  int <- valueMinisatVar solver k
  print $ "Value of var " ++ (show k) ++ ": " ++ (show $ fromIntegral int)
  if (k /= 0) then printVars solver (n - 1) else return ()

-- | Get the value of a variable in the solver.
getVarValue :: Solver -> Word -> IO (Int)
getVarValue solver n =
  do
  val <- valueMinisatVar solver (fromIntegral n)
  return $ fromIntegral val

-- | Add a clause to a veclit
addToVecLit :: Ptr MinisatVecLit -> [Lit] -> IO ()
addToVecLit veclit clause =
  case clause of
  [] -> return ()
  (v:vs) -> do
            case v of
              Var int ->
                pushMinisatVar
                veclit
                (fromIntegral (int * 2))
                (fromIntegral 1)
              Neg int ->
                pushMinisatVar
                veclit
                (fromIntegral (int * 2))
                (fromIntegral 0)
              Var' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                (fromIntegral 1)
              Neg' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                (fromIntegral 0)
            addToVecLit veclit vs

-- | Add a clause to a solver.
addClause :: Solver -> [Lit] -> IO CInt
addClause solver clause =
  do
  veclit <- newMinisatVecLit
  addToVecLit veclit clause
  addMinisatClause solver veclit
  deleteMinisatVecLit veclit
  return $ fromIntegral 0

-- | Add a list of clauses to a solver.
addClauses :: Solver -> ([[Lit]] -> IO ())
addClauses solver = mapM_ (addClause solver)

-- | Have a solver solve with the clauses that have been added.
solve :: Solver -> IO Bool
solve solver =
  do
  ok <- simplifyMinisat solver
  if ok == 0
  then return False
  else do
       res <- solveMinisat solver
       return $ res /= 0

-- | Have a solver solve with the clauses that have been added
-- along with given assumptions
solveWithAssumps :: Solver -> [Lit] -> IO Bool
solveWithAssumps solver assumps =
  do
  ok <- simplifyMinisat solver
  if ok == 0
  then return False
  else do
       veclit <- newMinisatVecLit
       addToVecLit veclit assumps
       addMinisatClause solver veclit
       res <- solveMinisatWithAssumps solver veclit
       return $ res /= 0

--------------------
-- Relevant Types --
--------------------

type Solver = Ptr MinisatSolver

data MinisatSolver = MinisatSolver

data MinisatVecLit = MinisatVecLit

type MinisatVar = CInt

-----------------
-- C Functions --
-----------------

foreign import ccall unsafe "newMinisatSolver"
  newSolver :: IO Solver

foreign import ccall unsafe "deleteMinisatSolver"
  deleteSolver :: Solver -> IO ()

foreign import ccall unsafe "newMinisatVar"
  newMinisatVar :: Solver -> CInt -> CInt -> IO (Ptr MinisatVar)

foreign import ccall unsafe "releaseMinisatVar"
  releaseMinisatVar :: Solver -> MinisatVar -> IO ()

foreign import ccall unsafe "addMinisatClause"
  addMinisatClause :: Solver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall unsafe "simplifyMinisat"
  simplifyMinisat :: Solver -> IO CInt

foreign import ccall unsafe "solveMinisatWithAssumps"
  solveMinisatWithAssumps :: Solver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall unsafe "solveMinisat"
  solveMinisat :: Solver -> IO CInt

foreign import ccall unsafe "valueMinisatVar"
  valueMinisatVar :: Solver -> CInt -> IO CInt

foreign import ccall unsafe "newMinisatVecLit"
  newMinisatVecLit :: IO (Ptr MinisatVecLit)

foreign import ccall unsafe "deleteMinisatVecLit"
  deleteMinisatVecLit :: Ptr MinisatVecLit -> IO ()

foreign import ccall unsafe "pushMinisatVar"
  pushMinisatVar :: Ptr MinisatVecLit -> MinisatVar -> CInt -> IO ()

foreign import ccall unsafe "printMinisatStats"
  printMinisatStats :: Solver -> IO ()
{-
main :: IO ()
main =
  do
  solver <- newSolver
  printMinisatStats solver
  addVars solver 4
  addClauses solver [[Var 1], [Neg 0], [Var 0, Var 1]]
  res <- solve solver
  printMinisatStats solver
  printVars solver 4
  print res
  deleteSolver solver
-}
