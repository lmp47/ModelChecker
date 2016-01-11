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

-- | Initiation phase of the algorithm
initiation :: Model -> Clause -> [Frame] -> Maybe Frame
initiation model prop [] =
    base (addTransitionToFrame (getFrame (vars model) $ initial model) model) prop
initiation model prop (f:fs) = base f prop

-- | Print the clauses per frame in the provided list of frames
printFrames :: [Frame] -> IO ()
printFrames [f] = print (show $ clauses f)
printFrames (f:fs) = print ("Frame " ++ (show $ length fs)) >> print (show $ clauses f) >> printFrames fs
printFrames [] = print "No frames in list."

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
consecution :: Model -> Lit -> Frame -> Bool
consecution model prop frame =
  consec frame [] 100
  where
    -- Limit n on the number of times consec can be invoked
    consec f acc 0 = unsafePerformIO (
                      do
                       --return (propagate (acc ++ [f]))
                       --printFrames (reverse $ acc ++ [f])
                       error "Limit of consec invocations reached." )
    consec f acc n =
      if nextHas f [prop]
      then pushFrame f (getFrame (vars model) []) acc n
      else
        let cti = nextCTI f [prop] model in
          case ctiFound f (fst $ currentNext cti) acc [prop] of
            (True, f', acc') -> if (clauses f' /= clauses f)
                                  then case propagate (acc' ++ [f']) of
                                    Just fs -> consec (fs !! (length fs - 1)) (take (length fs - 1) fs) (n - 1)
                                    Nothing -> True
                                  else False
            (False, f', acc') -> False
    pushFrame f f' acc n =
        case push f model f' of
        (True, _) -> True
        (False, f'') -> consec f'' (acc ++ [f]) (n - 1)
    propagate (f:f':frames) =
      case push f model f' of
        (True, f'') -> Nothing
        (False, f'') -> case propagate (f'':frames) of
                          Just fs -> Just (f:fs)
                          Nothing -> Nothing
    propagate frames = Just frames
    -- Try to prove CTI unreachable at current depth, given the negated CTI
    ctiFound f _ [] p = (False, f, [])
    ctiFound f cti acc p =
      if ( satisfiable (solveWithAssumps (solver (addTransitionToFrame (getFrame (vars model) (map neg cti:clauses (head acc))) model)) (map prime cti)) ||
           not (satisfiable (solve (solver (addTransitionToFrame (getFrame (vars model) (map neg cti:clauses (head acc))) model )))))
        then (False, f, acc)
        else case pushCTI (map neg cti) acc f of
               (Nothing, acc', f', []) -> if nextHas f' p
                                           then (True, f', acc')
                                           else ctiFound f' (fst (currentNext (nextCTI f' p model))) acc' p
               (Just model, acc', f', fs) -> case ctiFound f' (fst (currentNext model)) acc' (map neg cti) of
                                               (True, f'', acc'') -> (ctiFound f cti (acc'' ++ f'':(take (length fs - 1) fs)) p)
                                               false              -> false
    pushCTI negCTI [] f = (Nothing, [], f, [])
    pushCTI negCTI acc f =
      let res = solveWithAssumps (solver (addTransitionToFrame (getFrame (vars model) (negCTI:clauses (acc !! (length acc - 1)))) model)) (map (prime.neg) negCTI) in
        if not (satisfiable res)
          then (Nothing, (map (`addClauseToFrame` negCTI) acc), addClauseToFrame f negCTI, [])
          else
              case pushCTI negCTI (take (length acc - 1) acc) (acc !! (length acc - 1)) of
                (Just m, acc', f', leftover)  -> (Just m, acc', f', leftover ++ [f])
                (Nothing, acc', f', []) -> (Just (nextCTI (acc !! (length acc - 1)) negCTI model), acc', f', [f])

-- | Finds a CTI given a safety property clause
nextCTI :: Frame -> Clause -> Model -> [Lit]
nextCTI frame prop m =
  case res of
    Nothing -> error "No CTI found."
    Just lits ->
      if (satisfiable $ solveWithAssumps (solver (addTransitionToFrame (getFrame (vars m) (clauses frame)) m)) (map (prime.neg) prop ++ getAssignment frame (lits ++ map (prime.neg) prop)))
        then lits
        else error "Incorrect model" --error ("Could not assign: " ++ (show $ getAssignment frame (map (prime.neg) prop)))
  where
    res =  model $ solveWithAssumps (solver (addTransitionToFrame (getFrame (vars m) (clauses frame)) m)) (map (prime.neg) prop)

getAssignment frame lits = assign (unassigned []) []
  where
    unassigned assigned = case getUnassigned (solver frame) (assigned ++ lits) of
                            Just ls -> ls
                            Nothing -> error "Could not assign."
    assign [] assigned = assigned
    assign (c:cs) assigned = if satisfiable (solveWithAssumps (solver frame) (c:assigned ++ lits))
                               then assign (unassigned (c:assigned)) (c:assigned)
                               else assign (unassigned (neg c:assigned)) (neg c:assigned)

-- | Push clauses to next frame
push :: Frame -> Model -> Frame -> (Bool, Frame)
push f model =
  pusher (clauses f) True
  where
    pusher (c:cs) b f' = 
      if (c `notElem` clauses f') && nextHas f c
      then pusher cs b (addClauseToFrame f' c)
      else pusher cs (b && (nextHas f c)) f'
    pusher _ b f' = (b, addTransitionToFrame f' model)

-- | Checks if I && (not P) is UNSAT
base :: Frame -> Clause -> Maybe Frame
base f property =
  if satisfiable (solveWithAssumps (solver f) (map neg property))
    then Nothing
    else Just newFrame
  where
    newFrame = addClauseToFrame f property

-- | Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals
nextHas :: Frame -> Clause -> Bool
nextHas f =
  next (solver f)
  where
    next solver ls = not (satisfiable (solveWithAssumps solver (map (prime.neg) ls)))
