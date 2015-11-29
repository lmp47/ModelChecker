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
                             , clauses = if c `notElem` clauses f then c:clauses f else clauses f}

getFrame vars clauses = Frame { solver = addClauses (addVars newSolver vars) clauses
                              , clauses = clauses }

-- Keep transitions out of the clauses list
addTransitionToFrame f' model = Frame { solver = addClauses (solver f') (transition model)
                                      , clauses = clauses f' }

-- | Given a model and a safety property, checks if the model satisfies the property
prove :: Model -> Lit -> Bool
prove model prop =
  case initiation model prop [] of
    Just frame -> consecution model prop frame
    Nothing -> False

-- | Initiation phase of the algorithm
initiation :: Model -> Lit -> [Frame] -> Maybe Frame
initiation model prop frames =
  case frames of
    [] -> base (addTransitionToFrame (getFrame (vars model) $ initial model) model) prop
    f:fs -> base f prop

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
consecution :: Model -> Lit -> Frame -> Bool
consecution model prop frame =
  consec frame []
  where
  consec f acc =
    if nextHas f [prop]
    then
      pushFrame f (getFrame (vars model) []) acc
    else
      case ctiFound f (getCTI f model prop) acc [prop] of
        (True, f', acc') -> (clauses f' /= clauses f) && consec f' acc'
        (False, f', acc') -> False
  pushFrame f f' acc =
      case push f model f' of
      (True, _) -> True
      (False, f'') -> consec f'' (acc ++ [f])
  -- Try to prove CTI unreachable at current depth
  ctiFound f _ [] p = (False, f, [])
  ctiFound f cti acc p =
    let negCTI = map neg cti
        (acc', f'') = proveNegCTI negCTI acc f
        f'          = snd (push (last acc') model f'') in
      -- Some issues here
      if nextHas f' p  && any ((`elem` clauses f') . (: [])) p
        then (True, f', acc')
        else case getConflictWithAssumps (solver f') (negCTI ++ map prime negCTI) of
             Just cex -> case ctiFound (last acc') negCTI (take (length acc' - 1) acc') cex of
                         (True, f'', fs) -> ctiFound f' cti (fs ++ [f''])  p
                         false -> false
             _        -> error ("Failing query succeeded: " ++ show (clauses f') ++ " and " ++ show negCTI)
  -- Prove as many literals in the negated CTI clause as possible
  proveNegCTI :: [Lit] -> [Frame] -> Frame -> ([Frame], Frame)
  proveNegCTI [] acc f = (acc, f)
  proveNegCTI (l:ls) acc f =
    case initiation model l acc of
    Nothing -> proveNegCTI ls acc f
    Just i -> if nextHas (last acc) [l]
                then proveNegCTI ls (i:tail acc) (addClauseToFrame f [l])
                else proveNegCTI ls (i:tail acc) f

-- | Get a counterexample to induction state
getCTI :: Frame -> Model -> Lit -> [Lit]
getCTI frame model prop =
    [x | x <- badCurrent, [x] `notElem` clauses frame]
  where
  -- Get unassigned variables
  unassigned = getUnassigned (solver frame) [prime $ neg prop] (vars model)
  -- Assign the unassigned variables to get a single state
  assign cs = case getConflictWithAssumps (solver frame) (prime (neg prop):cs) of
              Just (cl:cls) -> assign $ neg cl:filter (/= cl) cs
              Nothing -> cs
  -- A single bad state
  bad = getLiterals (solver frame) (prime (neg prop):assign unassigned) (vars model)
  -- The bad assignments in the current state
  badCurrent = fst (currentNext bad)

-- | Push clauses to next frame
push :: Frame -> Model -> Frame -> (Bool, Frame)
push f model =
  pusher (clauses f) True
  where
  pusher (c:cs) b f' = 
    if (c `notElem` clauses f') && nextHas f c
    then pusher cs b (addClauseToFrame f' c)
    else pusher cs False f'
  pusher _ b f' = (b, addTransitionToFrame f' model)

-- | Checks if I && (not P) is UNSAT
base :: Frame -> Lit -> Maybe Frame
base f property =
  if solveWithAssumps (solver f) [neg property]
  then Nothing
  else Just newFrame
  where
  newFrame = addClauseToFrame f [property]

-- | Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals
nextHas :: Frame -> Clause -> Bool
nextHas f =
  next (solver f)
  where
  next solver ls = not (solveWithAssumps solver (map (prime.neg) ls))
