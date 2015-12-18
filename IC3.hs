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
  case initiation model [prop] [] of
    Just frame -> consecution model prop frame
    Nothing -> False

-- | Get the smallest set of states given the current clauses
getSmallest :: Model -> Frame -> Frame
getSmallest model frame =
  addTransitionToFrame (getFrame (vars model) (map (:[]) $ fst $ currentNext $ getLiterals (solver frame) [])) model

-- | Initiation phase of the algorithm
initiation :: Model -> Clause -> [Frame] -> Maybe Frame
initiation model prop [] =
    base (getSmallest model $ addTransitionToFrame (getFrame (vars model) $ initial model) model) prop
initiation model prop (f:fs) = base f prop

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
        case ctiFound f (map neg $ getCTI f prop) acc [prop] of
          (True, f', acc') -> (clauses f' /= clauses f) && consec f' acc'
          (False, f', acc') -> False
    pushFrame f f' acc =
        case push f model f' of
        (True, _) -> True
        (False, f'') -> consec f'' (acc ++ [f])
    -- Try to prove CTI unreachable at current depth, given the negated CTI
    ctiFound f _ [] p = (False, f, [])
    ctiFound f negCTI acc p =
      case proveNegCTI negCTI acc f of
        Nothing -> nextCTI f acc negCTI p
        Just (acc', f'') ->
          let f' = snd (push (last acc') model f'') in
            if nextHas f' p
              then (True, f', acc')
            else nextCTI f' acc' negCTI p
    nextCTI frame acc negCTI p =
      case getConflictWithAssumps (solver (addTransitionToFrame (getFrame (vars model) (map prime p:map prime negCTI:negCTI:clauses frame)) model)) [] of
        Just cex -> case ctiFound (last acc) (map neg cex) (take (length acc - 1) acc) negCTI of
                      (True, f, fs) -> if nextHas f p then (True, addClauseToFrame frame negCTI, fs ++ [f]) else (False, f, fs) -- need to push @ higher levels
                      false -> false
        _        -> error ("Failing query succeeded: " ++ show (clauses frame) ++ " and " ++ show negCTI)
    -- Prove as many literals in the negated CTI clause as possible
    proveNegCTI :: Clause -> [Frame] -> Frame -> Maybe ([Frame], Frame)
    proveNegCTI clause acc f =
      case initiation model clause acc of
        Nothing -> Nothing
        Just i  -> if nextHas (last acc) clause
                     then Just (i:map (`addClauseToFrame` clause) (tail acc), addClauseToFrame f clause)
                     else Nothing

-- | Get a counterexample to induction state as a conjunction of literals
getCTI :: Frame -> Lit -> [Lit]
getCTI frame prop =
    [x | x <- badCurrent, [x] `notElem` clauses frame]
  where
    -- Get unassigned variables
    unassigned = getUnassigned (solver frame) [prime $ neg prop]
    -- Assign the unassigned variables to get a single state
    assign cs = case getConflictWithAssumps (solver frame) (prime (neg prop):cs) of
                Just (cl:cls) -> assign $ neg cl:filter (/= cl) cs
                Nothing -> cs
    -- A single bad state
    bad = getLiterals (solver frame) (prime (neg prop):assign unassigned)
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
base :: Frame -> Clause -> Maybe Frame
base f property =
  if solveWithAssumps (solver f) (map neg property)
  then Nothing
  else Just newFrame
  where
    newFrame = addClauseToFrame f property

-- | Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals
nextHas :: Frame -> Clause -> Bool
nextHas f =
  next (solver f)
  where
    next solver ls = not (solveWithAssumps solver (map (prime.neg) ls))
