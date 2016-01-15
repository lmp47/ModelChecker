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

addClauseToFrame :: Frame -> Clause -> Frame
addClauseToFrame f c = Frame { solver = addClause (solver f) c
                             , clauses = if c `notElem` clauses f then c:clauses f else clauses f }

getFrame :: Word -> [Clause] -> Frame
getFrame vars clauses = Frame { solver = addClauses (addVars newSolver vars) clauses
                              , clauses = clauses }

-- | Keep transitions out of the clauses list
addTransitionToFrame :: Frame -> Model -> Frame
addTransitionToFrame f' model = Frame { solver = addClauses (solver f') (transition model)
                                      , clauses = clauses f' }

-- | Get frame with given clauses and given model's transition relation and variables
getFrameWith :: [Clause] -> Model -> Frame
getFrameWith clauses model = addTransitionToFrame (getFrame (vars model) clauses) model

-- | Given a model and a safety property, checks if the model satisfies the property
prove :: Model -> Lit -> Bool
prove model prop =
  initiation f0 [prop] && prove' model prop (addClauseToFrame f0 [prop])
  where
    f0 = getFrameWith (initial model) model

-- | Initiation query. Checks if I && (not P) is UNSAT.
initiation :: Frame -> Clause -> Bool
initiation f prop = not (satisfiable (solveWithAssumps (solver f) (map neg prop)))

-- | Consecution query. Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals.
consecution :: Frame -> Clause -> Bool
consecution f prop =
  not (satisfiable (solveWithAssumps (solver f) (map (prime.neg) prop)))

-- | Print the clauses per frame in the provided list of frames
printFrames :: [Frame] -> IO ()
printFrames [f] = print (show $ clauses f)
printFrames (f:fs) = print ("Frame " ++ show (length fs)) >> print (show $ clauses f) >> printFrames fs
printFrames [] = print "No frames in list."

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
prove' :: Model -> Lit -> Frame -> Bool
prove' model prop frame =
  consec frame [] 100
  where
    -- Limit n on the number of times consec can be invoked
    consec f acc 0 = error "Limit of consec invocations reached."
    consec f acc n =
      if consecution f [prop]
        then pushFrame f (getFrame (vars model) []) acc n
        else
          let cti = nextCTI f [prop] model in
            case ctiFound f (fst $ currentNext cti) acc [prop] of
              (True, f', acc') -> (clauses f' /= clauses f) &&
                                    (case propagate (acc' ++ [f']) of
                                       Just fs -> consec (fs !! (length fs - 1)) (take (length fs - 1) fs) (n - 1)
                                       Nothing -> True)
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
    -- Try to prove CTI unreachable at current depth
    ctiFound f _ [] p = (False, f, [])
    ctiFound f cti acc p =
      if satisfiable (solveWithAssumps (solver (getFrameWith (map neg cti:clauses (head acc)) model)) (map prime cti))
        then (False, f, acc)
        else
          case pushCTI (map neg cti) acc f of
            (Nothing, acc', f', []) -> if consecution f' p
                                         then (True, f', acc')
                                         else ctiFound f' (fst (currentNext (nextCTI f' p model))) acc' p
            (Just model, acc', f', fs) -> case ctiFound f' (fst (currentNext model)) acc' (map neg cti) of
                                            (True, f'', acc'') -> ctiFound f cti
                                                                    (acc'' ++ f'':take (length fs - 1) fs) p
                                            false              -> false
    -- Find the deepest frame where the negated CTI holds
    pushCTI negCTI [] f = (Nothing, [], f, [])
    pushCTI negCTI acc f =
      let res = solveWithAssumps
                  (solver (getFrameWith (negCTI:clauses (acc !! (length acc - 1))) model))
                  (map (prime.neg) negCTI) in
        if not (satisfiable res)
          then let negCTI' = inductiveGeneralization negCTI (head acc) f in
            (Nothing, map (`addClauseToFrame` negCTI') acc, addClauseToFrame f negCTI', [])
          else
            case pushCTI negCTI (take (length acc - 1) acc) (acc !! (length acc - 1)) of
              (Just m, acc', f', leftover)  -> (Just m, acc', f', leftover ++ [f])
              (Nothing, acc', f', []) -> (Just (nextCTI (acc !! (length acc - 1)) negCTI model), acc', f', [f])

-- | Find a minimal subclause of the provided clause that satisfies initiation and
-- consecution.
inductiveGeneralization clause f0 fk = clause --generalize clause f0 fk []
  where
    -- May want to limit number of attempts and find an approximate minimal subclause instead
    generalize [] _ _ needed = needed
    generalize (c:cs) f0 fk needed =
      if initiation f0 cs && consecution fk cs
        then generalize cs f0 fk needed
        else generalize cs f0 fk (c:needed)

-- | Finds a CTI given a safety property clause
nextCTI :: Frame -> Clause -> Model -> [Lit]
nextCTI frame prop m =
  case res of
    Just ls -> ls
    _       -> error "No CTI found."
  where
    res = model $ solveWithAssumps (solver (getFrameWith (clauses frame) m)) (map (prime.neg) prop)

-- | Push clauses to next frame
push :: Frame -> Model -> Frame -> (Bool, Frame)
push f model =
  pusher (clauses f) True
  where
    pusher (c:cs) b f' = 
      if (c `notElem` clauses f') && consecution f c
      then pusher cs b (addClauseToFrame f' c)
      else pusher cs (b && consecution f c) f'
    pusher _ b f' = (b, addTransitionToFrame f' model)
