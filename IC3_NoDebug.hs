{- |
Module : IC3
Description : Model checking algorithm
-}

module IC3 ( prove
           , getFrame
           , getFrameWith
           , addClauseToFrame
           , initiation
           , consecution
           , nextCTI
           , push ) where

import Model.Model
import Minisat.Minisat
import Data.Word
import System.IO.Unsafe
import Data.List

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
    initiation f0 [prop] && prove' model prop (addClauseToFrame f0 [prop]) []
  where
    f0 = getFrameWith (initial model) model

-- | Initiation query. Checks if I && (not P) is UNSAT.
initiation :: Frame -> Clause -> Bool
initiation f prop = not (satisfiable (solveWithAssumps (solver f) (map neg prop)))

-- | Consecution query. Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals.
consecution :: Frame -> Clause -> Bool
consecution f prop = not (satisfiable (solveWithAssumps (solver f) (map (prime.neg) prop)))

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
prove' :: Model -> Lit -> Frame -> [Frame] -> Bool
prove' m prop frame acc =
  if consecution frame [prop]
    then pushFrame frame (getFrame (vars m) []) acc
    else
      let cti = nextCTI frame [prop] m in
        case proveNegCTI m frame (fst $ currentNext cti) acc [prop] [] of
          (True, frame', acc', _) -> (clauses frame' /= clauses frame) &&
                                     (case propagate (acc' ++ [frame']) of
                                     Just fs ->
                                       prove' m prop (fs !! (length fs - 1))
                                         (take (length fs - 1) fs)
                                     Nothing -> True)
          (False, frame', acc', _) -> False
  where
    -- Push all possible clauses from frame f to frame f'
    pushFrame f f' acc =
      case push f m f' of
        (True, _) -> True
        (False, f'') -> prove' m prop f'' (acc ++ [f])
    -- Push all clauses and see if fixed point has been reached (resulting in Nothing)
    propagate (f:f':frames) =
      case push f m f' of
        (True, f'') -> Nothing
        (False, f'') -> case propagate (f'':frames) of
                          Just fs -> Just (f:fs)
                          Nothing -> Nothing
    propagate frames = Just frames

-- | Try to prove CTI unreachable at current depth given the current frame, CTI, previous frames, property, and later frames
proveNegCTI :: Model -> Frame -> [Lit] -> [Frame] -> Clause -> [Frame] -> (Bool, Frame, [Frame], [Frame])
proveNegCTI m f _ [] p fs = (False, f, [], fs)
proveNegCTI m f cti acc p fs =
    if satisfiable (solveWithAssumps (solver (getFrameWith (map neg cti:clauses (head acc)) m)) (map prime cti))
      then (False, f, acc, fs)
      else
        case pushNegCTI (map neg cti) acc f fs of
          (Nothing, acc', f', fs') -> if consecution f' p
                                        then (True, f', acc', fs')
                                        else proveNegCTI m f' (fst (currentNext (nextCTI f' p m))) acc' p fs'
          (Just model, acc', f', fs') -> case proveNegCTI m f' (fst (currentNext model)) acc' (map neg cti) fs' of
                                           (True, f'', acc'', fs'') -> proveNegCTI m f cti (acc'' ++ [f'']) p fs''
                                           false                    -> false
  where
    pushNegCTI negCTI [] f fs = (Nothing, [], f, fs)
    pushNegCTI negCTI acc f fs =
      let res = solveWithAssumps
                  (solver (getFrameWith (negCTI:clauses (acc !! (length acc - 1))) m))
                  (map (prime.neg) negCTI) in
        if not (satisfiable res)
          then let (negCTI', bfs, f':fs') = inductiveGeneralization negCTI acc f fs m 3 3 in
            (Nothing, map (`addClauseToFrame` negCTI') bfs, addClauseToFrame f negCTI', fs)
          else let f' = acc !! (length acc - 1) in
            (Just (nextCTI f' negCTI m), take (length acc - 1) acc, f', fs)

pushNegCTG :: Clause -> [Frame] -> [Frame] -> Model -> ([Frame], [Frame])
pushNegCTG negCTG acc [] _ = (acc, [])
pushNegCTG negCTG acc (f:fs) m =
  let res = solveWithAssumps
              (solver (getFrameWith (negCTG:clauses f) m))
              (map (prime.neg) negCTG) in
    if not (satisfiable res)
      then pushNegCTG negCTG (acc ++ [f]) fs m
      else (acc, f:fs)

-- | Find an approximate minimal subclause of the provided clause that satisfies initiation
-- and consecution and adds to the last frame
inductiveGeneralization :: Clause -> [Frame] -> Frame -> [Frame] -> Model -> Word -> Word -> (Clause, [Frame], [Frame])
inductiveGeneralization clause bfs fc afs m w w' = generalize clause bfs fc afs [] w w'
  where
    generalize cs bfs fc afs needed 0 _ = (cs ++ needed, bfs, fc:afs)
    generalize cs bfs fc afs needed _ 0 = (cs ++ needed, bfs, fc:afs)
    generalize [] bfs fc afs needed _ _ = (needed, bfs, fc:afs)
    generalize (l:ls) bfs fc afs needed k r = 
      case down (ls ++ needed) bfs (fc:afs) r of
        Just (fs', ls', r') -> let (fc':afs') = drop (length bfs) fs' in
                                 generalize (ls' \\ needed) (take (length bfs) fs') fc' afs' needed k r'
        Nothing -> generalize ls bfs fc afs (l:needed) ( k - 1 ) r 
    down ls (f0:fs) (fc:afs) r = 
      -- Check initiation and consecution for the potential generalization
      let init = initiation f0 ls
          consec = solveWithAssumps (solver (getFrameWith (ls:(clauses fc)) m)) (map (prime.neg) ls) in
      if not(init) || r == 0
        then Nothing
        else
          if not (satisfiable consec)
            then Just (f0:fs ++ fc:afs, ls, r) -- Generalization succeeded
            else
              case model consec of
                Just s ->  -- Try to push negCTG as far as possible
                  let negCTG = map neg (fst (currentNext s)) in
                    if not(null fs) && initiation f0 negCTG && consecution (last fs) negCTG
                      then
                        let rest = take (length afs) (fc:afs) -- all but frontier frame
                            lastf = last (fc:afs) -- frontier frame
                            (ctgs, nctgs) = pushNegCTG negCTG [] rest m
                            ctgs' = f0:fs ++ ctgs
                            (fdps, fd) = (take (length ctgs' - 1) ctgs', last ctgs') -- fd is deepest frame with negCTI inductive
                            (c, bfs', fs') = generalize negCTG fdps fd (nctgs ++ [lastf]) [] ( w - 1 ) ( r - 1 ) 
                            bfs'' = if (length bfs' <= length fdps)
                                      then map (`addClauseToFrame` c) bfs'
                                      else let fd':abfs = (drop (length fdps + 1) bfs') in
                                             (map (`addClauseToFrame` c) (take (length fdps) bfs')) ++ (addClauseToFrame fd' c:abfs)
                            fc':afs' = let l = 1 + length fdps - length bfs'' in
                                         if l > 0 
                                           then let (fd':aafs) = (drop l fs') in
                                                  (map (`addClauseToFrame` c) (take l fs')) ++ (addClauseToFrame fd' c:aafs)
                                           else fs' in
                              down ls bfs'' (fc':afs') ( r - 1 )
                      else down (ls `intersect` (map neg s)) (f0:fs) (fc:afs) ( r - 1 )
                _ -> error "Could not find predecessor when finding MIC"

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
push f model f' =
  pusher (clauses f \\ clauses f') True f'
  where
    pusher (c:cs) b f' = 
      if consecution f c
      then pusher cs b (addClauseToFrame f' c)
      else pusher cs False f'
    pusher _ b f' = (b, addTransitionToFrame f' model)
