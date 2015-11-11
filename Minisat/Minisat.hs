{-|
Module: Minisat
Description: Haskell bindings for some Minisat functions

Haskell bindings for some Minisat functions.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Minisat.Minisat ( Solver
                       , newSolver
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
import Foreign.ForeignPtr
import System.IO.Unsafe

-----------------------
-- Wrapper Functions --
-----------------------

type Solver = IO (ForeignPtr MinisatSolver)

newSolver :: Solver
newSolver = newMinisatSolver >>= newForeignPtr deleteMinisatSolver

-- | Adds variables 0 to n - 1 to the solver.
addVars :: Solver -> Word -> Solver
addVars solver' n =
  do
  solver <- solver'
  withForeignPtr solver (addVars' n)
  return solver
  where
  addVars' n solver =
    if n > 0 then do
      newMinisatVar solver (fromIntegral $ n - 1) 0
      addVars' (n - 1) solver
    else return solver

-- | Prints variables 0 to n - 1 in the solver.
printVars :: Solver -> Word -> IO ()
printVars solver' n =
  let k = fromIntegral $ n - 1 in
  do
  solver <- solver'
  int <- withForeignPtr solver (\x -> valueMinisatVar x k)
  print $ "Value of var " ++ (show k) ++ ": " ++ (show $ fromIntegral int)
  if (k /= 0) then printVars solver' (n - 1) else return ()

-- | Get the value of a variable in the solver.
getVarValue :: Solver -> Word -> Int
getVarValue solver' n = unsafePerformIO res
  where
  res = do
        solver <- solver'
        val <- withForeignPtr solver (\x -> valueMinisatVar x (fromIntegral n))
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
addClause :: Solver -> [Lit] -> Solver
addClause solver' c =
  do
  solver <- solver'
  withForeignPtr solver (\x -> addClause' x c)
  return solver

-- | Add a list of clauses to a solver.
addClauses :: Solver -> ([[Lit]] -> Solver)
addClauses solver' cs = 
  do
  solver <- solver'
  withForeignPtr solver (\x -> mapM_ (addClause' x) cs)
  return solver


addClause' :: Ptr MinisatSolver -> [Lit] -> IO (Ptr MinisatSolver)
addClause' solver clause =
  do
  veclit <- newMinisatVecLit
  addToVecLit veclit clause
  addMinisatClause solver veclit
  deleteMinisatVecLit veclit
  return solver


-- | Have a solver solve with the clauses that have been added.
solve :: Solver -> Bool
solve solver' = unsafePerformIO res
  where
  res =
    do
    solver <- solver'
    ok <- withForeignPtr solver simplifyMinisat
    if ok == 0
    then return False
    else do
         res <- withForeignPtr solver solveMinisat
         return $ res /= 0


-- | Have a solver solve with the clauses that have been added
-- along with given assumptions
solveWithAssumps :: Solver -> [Lit] -> Bool
solveWithAssumps solver' assumps = unsafePerformIO res
  where
  res = do
        solver <- solver'
        ok <- withForeignPtr solver simplifyMinisat
        if ok == 0
        then return False
        else withForeignPtr solver
             (\solver ->
             do
             veclit <- newMinisatVecLit
             addToVecLit veclit assumps
             addMinisatClause solver veclit
             res <- solveMinisatWithAssumps solver veclit
             return $ res /= 0)


--------------------
-- Relevant Types --
--------------------

--type Solver = Ptr MinisatSolver

data MinisatSolver = MinisatSolver

data MinisatVecLit = MinisatVecLit

type MinisatVar = CInt

-----------------
-- C Functions --
-----------------

foreign import ccall unsafe "newMinisatSolver"
  newMinisatSolver :: IO (Ptr MinisatSolver)

foreign import ccall unsafe "&deleteMinisatSolver"
  deleteMinisatSolver :: FinalizerPtr MinisatSolver

foreign import ccall unsafe "newMinisatVar"
  newMinisatVar :: Ptr MinisatSolver -> CInt -> CInt -> IO (Ptr MinisatVar)

foreign import ccall unsafe "releaseMinisatVar"
  releaseMinisatVar :: Ptr MinisatSolver -> MinisatVar -> IO ()

foreign import ccall unsafe "addMinisatClause"
  addMinisatClause :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall unsafe "simplifyMinisat"
  simplifyMinisat :: Ptr MinisatSolver -> IO CInt

foreign import ccall unsafe "solveMinisatWithAssumps"
  solveMinisatWithAssumps :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall unsafe "solveMinisat"
  solveMinisat :: Ptr MinisatSolver -> IO CInt

foreign import ccall unsafe "valueMinisatVar"
  valueMinisatVar :: Ptr MinisatSolver -> CInt -> IO CInt

foreign import ccall unsafe "newMinisatVecLit"
  newMinisatVecLit :: IO (Ptr MinisatVecLit)

foreign import ccall unsafe "deleteMinisatVecLit"
  deleteMinisatVecLit :: Ptr MinisatVecLit -> IO ()

foreign import ccall unsafe "pushMinisatVar"
  pushMinisatVar :: Ptr MinisatVecLit -> MinisatVar -> CInt -> IO ()

foreign import ccall unsafe "printMinisatStats"
  printMinisatStats :: Ptr MinisatSolver -> IO ()
