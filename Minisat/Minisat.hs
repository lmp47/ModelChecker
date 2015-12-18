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
                       , getLiterals
                       , getUnassigned
                       , getConflictWithAssumps
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
import Control.Monad
import Foreign.Marshal.Array

-----------------------
-- Wrapper Functions --
-----------------------

data Solver = Solver { solver  :: IO (ForeignPtr MinisatSolver)
                     , numVars :: Word }

newSolver :: Solver
newSolver = Solver { solver = newMinisatSolver >>= newForeignPtr deleteMinisatSolver
                   , numVars = 0 }

-- | Adds variables 0 to n - 1 to the solver.
addVars :: Solver -> Word -> Solver
addVars s n =
  Solver { solver  = addVars' (solver s) n
         , numVars = max (numVars s) n }
  where
    addVars' solver' n =
      do
      solver <- solver'
      withForeignPtr solver (addVars'' n)
      return solver
    addVars'' n solver =
      if n > 0
        then do
          newMinisatVar solver (fromIntegral $ n - 1) 0
          addVars'' (n - 1) solver
        else return solver

-- | Prints all the variables in the solver.
printVars :: Solver -> IO ()
printVars s =
  printVars' (solver s) (numVars s)
  where
    printVars' :: IO (ForeignPtr MinisatSolver) -> Word -> IO ()
    printVars' s n =
      let k = fromIntegral $ n - 1 in
      do
      solver' <- s
      veclit <- newMinisatVecLit
      int <- withForeignPtr solver' (\x -> valueMinisatVar x veclit k)
      print $ "Value of var " ++ show k ++ ": " ++ show (fromIntegral int)
      when (k /= 0) $ printVars' s (n - 1)

-- | Get the value of a variable in the solver.
getVarValue :: Solver -> [Lit] -> Word -> Int
getVarValue s assumps n = unsafePerformIO res
  where
    res = do
          return $ solve s
          solver' <- solver s
          veclit <- newMinisatVecLit
          addToVecLit veclit assumps
          val <- withForeignPtr solver' (\x -> valueMinisatVar x veclit (fromIntegral n))
          return $ fromIntegral val

-- | Get the true literals in the solver.
getLiterals :: Solver -> [Lit] -> [Lit]
getLiterals s assumps = getLits (numVars s - 1)
  where
  getLits k = case getLit k (getVarValue s assumps k) of
              Nothing -> if k == 0 then [] else getLits (k - 1)
              Just l  -> if k == 0 then [l] else l:getLits (k - 1)
  getLit k val = case (k `rem` 2, val) of
                  (0,0) -> Just (Neg  (k `div` 2))
                  (0,1) -> Just (Var  (k `div` 2))
                  (1,0) -> Just (Neg' (k `div` 2))
                  (1,1) -> Just (Var' (k `div` 2))
                  _     -> Nothing

-- | Get the unassigned variables in the solver.
getUnassigned :: Solver -> [Lit] -> [Lit]
getUnassigned s assumps = getLits (numVars s - 1)
  where
  getLits k = case getLit k (getVarValue s assumps k) of
              (Nothing) -> if k == 0 then [] else getLits (k - 1)
              (Just l ) -> if k == 0 then [l] else l:getLits (k - 1)
  getLit k val = case (k `rem` 2, val) of
                  (0,2) -> Just (Neg  (k `div` 2))
                  (1,2) -> Just (Neg' (k `div` 2))
                  _     -> Nothing

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
                1
              Neg int ->
                pushMinisatVar
                veclit
                (fromIntegral (int * 2))
                0
              Var' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                1
              Neg' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                0
            addToVecLit veclit vs

-- | Add a clause to a solver.
addClause :: Solver -> [Lit] -> Solver
addClause s c =
  Solver { solver =
             do
             solver' <- solver s
             withForeignPtr solver' (`addClause'` c)
             return solver'
         , numVars = numVars s }

-- | Add a list of clauses to a solver.
addClauses :: Solver -> [[Lit]] -> Solver 
addClauses s cs = 
  Solver { solver = 
             do
             solver' <- solver s
             withForeignPtr solver' (\x -> mapM_ (addClause' x) cs)
             return solver'
         , numVars = numVars s }

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
solve s = unsafePerformIO res
  where
  res =
    do
    solver' <- solver s
    ok <- withForeignPtr solver' simplifyMinisat
    if ok == 0
      then return False
      else do
        res <- withForeignPtr solver' solveMinisat
        return $ res /= 0

-- | Have a solver solve with the clauses that have been added
-- along with given assumptions
solveWithAssumps :: Solver -> [Lit] -> Bool
solveWithAssumps s assumps = unsafePerformIO res
  where
  res = do
        solver' <- solver s
        ok <- withForeignPtr solver' simplifyMinisat
        if ok == 0
          then return False
          else withForeignPtr solver'
            (\solver ->
            do
            veclit <- newMinisatVecLit
            addToVecLit veclit assumps
            addMinisatClause solver veclit
            res <- solveMinisatWithAssumps solver veclit
            return $ res /= 0)

-- | Get unsat core
getConflictWithAssumps :: Solver -> [Lit] -> Maybe [Lit]
getConflictWithAssumps s assumps =
  if solveWithAssumps s assumps
    then Nothing
    else Just (unsafePerformIO conflict)
  where
  conflict = solver s >>= (`withForeignPtr` getLits)
  getLits ptr = do
    veclit <- newMinisatVecLit
    addToVecLit veclit assumps
    vec <- getMinisatConflictVec ptr veclit
    size <- getMinisatConflictSize ptr veclit
    liftM (map fromMinisatLit) (peekArray (fromIntegral size) vec)
  fromMinisatLit mLit = unsafePerformIO (
    do
    var <- liftM fromIntegral (varMinisatLit mLit) 
    val <- liftM fromIntegral (valueMinisatLit mLit)
    case (var `mod` 2 == 0, val == 0) of
      (True, True)   -> return $ Var  (var `div` 2)
      (True, False)  -> return $ Neg  (var `div` 2)
      (False, True)  -> return $ Var' (var `div` 2)
      (False, False) -> return $ Neg' (var `div` 2)
    )

--------------------
-- Relevant Types --
--------------------

data MinisatSolver = MinisatSolver

type MinisatVecLit = [CInt]

type MinisatLit = CInt

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

foreign import ccall unsafe "getMinisatConflictVec"
  getMinisatConflictVec :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO (Ptr CInt)

foreign import ccall unsafe "getMinisatConflictSize"
  getMinisatConflictSize :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall unsafe "valueMinisatLit"
  valueMinisatLit :: MinisatLit -> IO CInt

foreign import ccall unsafe "varMinisatLit"
  varMinisatLit :: MinisatLit -> IO MinisatVar

foreign import ccall unsafe "valueMinisatVar"
  valueMinisatVar :: Ptr MinisatSolver -> Ptr MinisatVecLit -> CInt -> IO CInt

foreign import ccall unsafe "newMinisatVecLit"
  newMinisatVecLit :: IO (Ptr MinisatVecLit)

foreign import ccall unsafe "deleteMinisatVecLit"
  deleteMinisatVecLit :: Ptr MinisatVecLit -> IO ()

foreign import ccall unsafe "pushMinisatVar"
  pushMinisatVar :: Ptr MinisatVecLit -> MinisatVar -> CInt -> IO ()

foreign import ccall unsafe "printMinisatStats"
  printMinisatStats :: Ptr MinisatSolver -> IO ()
