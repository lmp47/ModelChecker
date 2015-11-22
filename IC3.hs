{- |
Module : IC3
Description : Model checking algorithm
-}

module IC3 ( prove ) where

import Model.Model
import Minisat.Minisat
import Data.Word
import System.IO.Unsafe

data Frame = Frame { solver  :: Solver
                   , clauses :: [Clause] }

addClauseToFrame f c = Frame { solver = addClause (solver f) c
                             , clauses = c:clauses f }

getFrame vars clauses = Frame { solver = addClauses (addVars newSolver vars) clauses
                              , clauses = clauses }

-- Keep transitions out of the clauses list
addTransitionToFrame f' model = Frame { solver = addClauses (solver f') (transition model)
                                      , clauses = clauses f' }

-- todo?: Return separate result if failure occurs in the initiation step

-- | Given a model and a safety property, checks if the model satisfies the property
prove :: Model -> Lit -> Bool
prove model prop = unsafePerformIO ((print (show $ initial model)) >> return (fst $ prove' model prop [] False))

prove' :: Model -> Lit -> [Frame] -> Bool -> (Bool, [Frame])
prove' model prop frames lim =
  case initiation model prop frames of
    Just frame -> unsafePerformIO (print "prove' called" >> return (consecution model prop (frame:rest) lim))
    Nothing -> unsafePerformIO (print "initiation failed" >> return (False, frames))
  where
  rest = case frames of
         (f:fs) -> fs
         _ -> []

-- | Initiation phase of the algorithm
initiation :: Model -> Lit -> [Frame] -> Maybe Frame
initiation model prop frames =
  case frames of
    [] -> base (addTransitionToFrame (getFrame (vars model) $ initial model) model) prop
    f:fs -> base f prop

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
consecution :: Model -> Lit -> [Frame] -> Bool -> (Bool, [Frame])
consecution model prop frames lim =
  consec frames []
  where
  consec [] acc = (False, acc)
  consec (f:fs) acc =
    if nextHas f [prop]
    then
      case (lim, fs) of
      (True, [])  -> (True, acc ++ [f])
      (False, []) -> pushFrame f (getFrame (vars model) []) acc fs
      (_, f':fs')  -> pushFrame f f' acc fs'
    else
      proveOne (getCTI f (vars model)) (acc)
  pushFrame f f' acc fs =
      case push f model f' of
      (True, _) -> (True, acc ++ (f:f':fs))
      (False, f'') -> consec (f'':fs) (acc ++ [f])
  -- Can prove at least one of the Lits in the first argument?
  proveOne :: [Lit] -> [Frame] -> (Bool, [Frame])
  proveOne [] fs = (False, fs)
  proveOne _ [] = (False, [])
  proveOne (l:ls) fs =
    case prove' model l fs True of
      (True, fs') -> (True, fs')
      (False, fs') -> proveOne ls fs'

-- | Get a counterexample to induction state
getCTI :: Frame -> Word -> [Lit]
getCTI frame numVars =
  let res = getLiterals (solver frame) numVars in
    unsafePerformIO (print ("CTI: " ++ (show $ map neg res)) >> return res)

-- Push clauses to next frame
push :: Frame -> Model -> Frame -> (Bool, Frame)
push f model frame =
  pusher (clauses f) True frame
  where
  pusher (c:cs) b f' = 
    if nextHas f c
    then pusher cs b (addClauseToFrame f' c)
    else pusher cs False f'
  pusher _ b f' = (b, addTransitionToFrame f' model)

-- I => P : Need I && (not P) to be UNSAT
base :: Frame -> Lit -> Maybe Frame
base f property =
  if solveWithAssumps (solver f) [neg property]
  then Nothing
  else Just newFrame
  where
  newFrame = addClauseToFrame f [property]

-- F_k && T => P' : Need F_k && T && (not P') to be UNSAT
nextHas :: Frame -> Clause -> Bool
nextHas f =
  next (solver f)
  where
  next solver (l:ls) = not (solveWithAssumps solver [prime $ neg l]) && next solver ls
  next solver [] = True 
