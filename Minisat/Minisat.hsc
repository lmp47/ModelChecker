{-|
Module: Minisat
Description: Haskell bindings for some Minisat functions

Haskell bindings for some Minisat functions.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Minisat.Minisat ( Solver
                       , Result ( satisfiable, model, conflict )
                       , newSolver
                       , addVars
                       , getVarValue
                       , getLiterals
                       , getUnassigned
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
import Data.List

#include "CSolver.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-----------------------
-- Wrapper Functions --
-----------------------

-- | Get a new solver.
newSolver :: Solver
newSolver = Solver { solver   = newMinisatSolver >>= newForeignPtr deleteMinisatSolver
                   , numVars  = 0 }

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
          newMinisatVar solver (fromIntegral $ n - 1) 1
          addVars'' (n - 1) solver
        else return solver

-- | Get the value of a variable in the solver.
getVarValue :: Solver -> [Lit] -> Word -> Int
getVarValue s assumps n
  | fromMinisatLit (fromIntegral n) `elem` lits = 0
  | neg (fromMinisatLit (fromIntegral n)) `elem` lits = 1
  | otherwise = 2
  where
    lits = case model (solveWithAssumps s assumps) of
             Nothing -> []
             Just ls -> ls

-- | Get the true literals in the solver.
getLiterals :: Solver -> [Lit] -> [Lit]
getLiterals s assumps =
  case model (solveWithAssumps s assumps) of
  Nothing   -> []
  Just lits -> lits

-- | Get the unassigned variables in the solver.
getUnassigned :: Solver -> [Lit] -> Maybe [Lit]
getUnassigned s assumps =
  case lits of
    Just ls -> Just (vars \\ map makePositive ls)
    Nothing -> Nothing
  where
  makePositive (Neg v) = Var v
  makePositive (Neg' v) = Var' v
  makePositive pos = pos
  lits = model (solveWithAssumps s assumps)
  varNums  = [0..(numVars s `div` 2)- 1]
  vars  = map Var varNums ++ map Var' varNums

-- | Add a clause to a veclit.
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
                0
              Neg int ->
                pushMinisatVar
                veclit
                (fromIntegral (int * 2))
                1
              Var' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                0
              Neg' int ->
                pushMinisatVar
                veclit
                (fromIntegral $ (int * 2) + 1)
                1
            addToVecLit veclit vs

-- | Add a clause to a solver.
addClause :: Solver -> [Lit] -> Solver
addClause s c =
  Solver { solver  =
             do
             solver' <- solver s
             withForeignPtr solver' (`addClause'` c)
             return solver'
         , numVars = numVars s }

-- | Add a list of clauses to a solver.
addClauses :: Solver -> [[Lit]] -> Solver 
addClauses s cs = 
  Solver { solver  = 
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
solve :: Solver -> Result
solve s = solveWithAssumps s []

-- | Have a solver solve with the clauses that have been added
-- along with given assumptions.
solveWithAssumps :: Solver -> [Lit] -> Result
solveWithAssumps s assumps = unsafePerformIO res
  where
  res :: IO Result
  res = do
        solver' <- solver s
        ok <- withForeignPtr solver' simplifyMinisat
        if ok == 0
          then return (Result False Nothing Nothing)
          else withForeignPtr solver'
            (\sol ->
              do
                veclit <- newMinisatVecLit
                addToVecLit veclit assumps
                res <- solveMinisatWithAssumps sol veclit >>= peek
                return res)

-- | Convert from a minisat variable number to a positive literal of that variable.
fromMinisatLit :: MinisatLit -> Lit
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

-- | Solver type including the Minisat solver and the number of variables in the
-- solver.
data Solver = Solver { solver  :: IO (ForeignPtr MinisatSolver)
                     , numVars :: Word }

-- | The result returned by a call to a Minisat solve function.
-- The satisfiable Bool is the boolean value returned by Minisat.
-- The conflict list is the conflict vector returned by Minisat for an UNSAT result.
-- The model list is the assignments returned by Minisat for a SAT result.
data Result = Result { satisfiable :: Bool
                     , conflict    :: Maybe [Lit]
                     , model       :: Maybe [Lit] }

instance Storable Result where
  alignment _ = #{alignment result}
  sizeOf    _ = #{size result}
  peek ptr    = do
                res <- #{peek result, solved} ptr
                let solved = neqz res
                modelSize <- mSize
                conflictSize <- cSize
                model <- if solved
                           then
                             do ptr <- #{peek result, model} ptr
                                lbools <- (peekArray (fromIntegral modelSize) ptr)
                                return (Just (makeLits lbools modelSize))
                           else return Nothing
                conflict <- if solved
                              then return Nothing
                              else
                                do ptr <- #{peek result, conflict} ptr
                                   c <- liftM (map fromMinisatLit) (peekArray (fromIntegral conflictSize) ptr)
                                   return (Just c)
                return (Result solved conflict model)
                where
                  neqz :: CUInt -> Bool
                  neqz res = res /= 0
                  mSize :: IO CUInt
                  mSize = #{peek result, modelSize} ptr
                  cSize :: IO CUInt
                  cSize = #{peek result, conflictSize} ptr
                  makeLits :: [CUChar] -> CUInt -> [Lit]
                  makeLits lbools size =
                    let ts = filter (\x -> 0 == lbools !! fromIntegral x) [0..(fromIntegral size - 1)]
                        fs = filter (\x -> 1 == lbools !! fromIntegral x) [0..(fromIntegral size - 1)]
                        negs  = filter (\x -> x `mod` 2 == 0) fs
                        negs' = filter (\x -> x `mod` 2 == 1) fs
                        vars  = filter (\x -> x `mod` 2 == 0) ts
                        vars' = filter (\x -> x `mod` 2 == 1) ts
                    in
                      map (Neg.(`div` 2)) negs ++ map (Var.(`div` 2)) vars ++
                      map (Var'.(`div` 2)) vars' ++ map (Neg'.(`div` 2)) negs'
  poke _ _    = return ()

data MinisatSolver = MinisatSolver

type MinisatVecLit = [CInt]

type MinisatLit = CInt

type MinisatVar = CInt

-----------------
-- C Functions --
-----------------

foreign import ccall safe "newMinisatSolver"
  newMinisatSolver :: IO (Ptr MinisatSolver)

foreign import ccall safe "&deleteMinisatSolver"
  deleteMinisatSolver :: FinalizerPtr MinisatSolver

foreign import ccall safe "newMinisatVar"
  newMinisatVar :: Ptr MinisatSolver -> CInt -> CInt -> IO (Ptr MinisatVar)

foreign import ccall safe "releaseMinisatVar"
  releaseMinisatVar :: Ptr MinisatSolver -> MinisatVar -> IO ()

foreign import ccall safe "addMinisatClause"
  addMinisatClause :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO CInt

foreign import ccall safe "simplifyMinisat"
  simplifyMinisat :: Ptr MinisatSolver -> IO CInt

foreign import ccall safe "solveMinisatWithAssumps"
  solveMinisatWithAssumps :: Ptr MinisatSolver -> Ptr MinisatVecLit -> IO (Ptr Result)

foreign import ccall safe "solveMinisat"
  solveMinisat :: Ptr MinisatSolver -> IO CInt

foreign import ccall safe "valueMinisatLit"
  valueMinisatLit :: MinisatLit -> IO CInt

foreign import ccall safe "varMinisatLit"
  varMinisatLit :: MinisatLit -> IO MinisatVar

foreign import ccall safe "valueMinisatVar"
  valueMinisatVar :: Ptr MinisatSolver -> Ptr MinisatVecLit -> CInt -> IO CInt

foreign import ccall safe "newMinisatVecLit"
  newMinisatVecLit :: IO (Ptr MinisatVecLit)

foreign import ccall safe "deleteMinisatVecLit"
  deleteMinisatVecLit :: Ptr MinisatVecLit -> IO ()

foreign import ccall safe "pushMinisatVar"
  pushMinisatVar :: Ptr MinisatVecLit -> MinisatVar -> CInt -> IO ()

foreign import ccall safe "printMinisatStats"
  printMinisatStats :: Ptr MinisatSolver -> IO ()
