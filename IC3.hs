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
import Data.IORef
import Debug.Trace
import Data.Ord

ctiCount :: IORef Int
{-# NOINLINE ctiCount #-}
ctiCount = unsafePerformIO (newIORef 0)

ctgCount :: IORef Int
{-# NOINLINE ctgCount #-}
ctgCount = unsafePerformIO (newIORef 0)

queryCount :: IORef Int
{-# NOINLINE queryCount #-}
queryCount = unsafePerformIO (newIORef 0)

increment :: IORef Int -> IO ()
increment x = atomicModifyIORef' x increment'
  where
    increment' y = (y + 1, ())

zero :: IORef Int -> IO ()
zero x = atomicModifyIORef' x zero'
  where
    zero' y = (0, ())

readR :: IORef Int -> IO Int
readR x = atomicModifyIORef' x read'
  where
    read' x = (x, x)

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
  unsafePerformIO (zero ctiCount >> zero ctgCount >> zero queryCount >> return (
    initiation f0 [prop] && prove' model prop (addClauseToFrame f0 [prop]) []
  ))
  where
    f0 = getFrameWith (initial model) model

-- | Initiation query. Checks if I && (not P) is UNSAT.
initiation :: Frame -> Clause -> Bool
initiation f prop = unsafePerformIO (increment queryCount >> return (
                      not (satisfiable (solveWithAssumps (solver f) (map neg prop))) ))

-- | Consecution query. Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals.
consecution :: Frame -> Clause -> Bool
consecution f prop = unsafePerformIO (increment queryCount >> return (
  not (satisfiable (solveWithAssumps (solver f) (map (prime.neg) prop))) ))

-- | Print the clauses per frame in the provided list of frames
printFrames :: [Frame] -> IO ()
printFrames [f] = print (clauses f)
printFrames (f:fs) = print ("Frame " ++ show (length fs)) >> print (show $ clauses f) >> printFrames fs
printFrames [] = print "No frames in list."

-- | Calculate the average number of literals per clause in each frame
calcAvgLitsPerCls :: [Frame] -> Double
calcAvgLitsPerCls frames =
  sum (map calcAvgPerFrame frames) / fromIntegral (length frames)
  where
  calcAvgPerFrame f =
    let cls = clauses f in
      fromIntegral (sum (map length cls)) / fromIntegral (length cls)

-- | Trace stat output
stats :: [Frame] -> IORef Int -> IORef Int -> IORef Int -> a -> a
stats frames cc gc qc = trace ("Number of frames: " ++ show (length frames) ++
                            "\nAverage number of literals/clause (not counting transition relation): "
                            ++ show(calcAvgLitsPerCls frames) ++
                            "\nNumber of ctis: " ++ show (unsafePerformIO $ readR cc) ++ 
                            "\nNumber of ctgs: " ++ show (unsafePerformIO $ readR gc) ++ 
                            "\nNumber of queries: " ++ show (unsafePerformIO $ readR qc) )

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
prove' :: Model -> Lit -> Frame -> [Frame] -> Bool
prove' m prop frame acc =
  if consecution frame [prop]
    then pushFrame frame (getFrame (vars m) []) acc
    else
      let cti = nextCTI frame [prop] m in
        case proveNegCTI m frame (fst $ currentNext cti) acc [prop] [] of
          (True, frame', acc', _) -> case propagate (acc' ++ [frame']) of
                                     Just fs ->
                                       prove' m prop (fs !! (length fs - 1))
                                         (take (length fs - 1) fs)
                                     Nothing -> stats (frame:acc) ctiCount ctgCount queryCount True
          (False, frame', acc', _)-> stats (frame:acc) ctiCount ctgCount queryCount False
  where
    -- Push all possible clauses from frame f to frame f'
    pushFrame f f' acc =
      case push f m f' of
        (_, True, _) -> stats (frame:acc) ctiCount ctgCount queryCount True
        (f, False, f'') -> prove' m prop f'' (acc ++ [f])
    -- Push all clauses and see if fixed point has been reached (resulting in Nothing)
    propagate (f:f':frames) =
      case push f m f' of
        (_, True, f'') -> Nothing
        (f, False, f'') -> case propagate (f'':frames) of
                          Just fs -> Just (f:fs)
                          Nothing -> Nothing
    propagate frames = Just frames

-- | Try to prove CTI unreachable at current depth given the current frame, CTI, previous frames, property, and later frames
proveNegCTI :: Model -> Frame -> [Lit] -> [Frame] -> Clause -> [Frame] -> (Bool, Frame, [Frame], [Frame])
proveNegCTI m f _ [] p fs = (False, f, [], fs)
proveNegCTI m f cti acc p fs =
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
      let res = unsafePerformIO ( increment queryCount >> return (
                  solveWithAssumps
                  (solver (getFrameWith (negCTI:clauses (acc !! (length acc - 1))) m))
                  (map (prime.neg) negCTI) )) in
        if not (satisfiable res)
          then let (negCTI', bfs, f':fs') = inductiveGeneralization negCTI acc f fs m 3 3 in
            (Nothing, bfs, addClauseToFrame f negCTI', fs)
          else let f' = acc !! (length acc - 1) in
            (Just (nextCTI f' negCTI m), take (length acc - 1) acc, f', fs)

pushNegCTG :: Clause -> [Frame] -> [Frame] -> Model -> ([Frame], [Frame])
pushNegCTG negCTG acc [] _ = (acc, [])
pushNegCTG negCTG acc (f:fs) m =
  let res = unsafePerformIO ( increment queryCount >> return (
              solveWithAssumps
              (solver (getFrameWith (negCTG:clauses f) m))
              (map (prime.neg) negCTG) )) in
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
          consec = unsafePerformIO (increment queryCount >> return (
                     solveWithAssumps (solver (getFrameWith (ls:clauses fc) m)) (map (prime.neg) ls)
                   )) in
      if not init || r == 0
        then Nothing
        else
          if not (satisfiable consec)
            then Just (f0:fs ++ fc:afs, ls, r) -- Generalization succeeded
            else
              case model consec of
                Just s ->  -- Try to push negCTG as far as possible
                  let negCTG = map neg (fst (currentNext s)) in
                    if not(null fs) && initiation f0 negCTG && consecution (last fs) negCTG
                      then unsafePerformIO (increment ctgCount >> return (
                        let rest = take (length afs) (fc:afs) -- all but frontier frame
                            lastf = last (fc:afs) -- frontier frame
                            (ctgs, nctgs) = pushNegCTG negCTG [] rest m
                            ctgs' = f0:fs ++ ctgs
                            (fdps, fd) = (take (length ctgs' - 1) ctgs', last ctgs') -- fd is deepest frame with negCTI inductive
                            (c, bfs', fc':afs') = generalize negCTG fdps fd (nctgs ++ [lastf]) [] ( w - 1 ) ( r - 1 ) in
                              down ls bfs' (addClauseToFrame fc' c:afs') ( r - 1 )))
                      else down (ls `intersect` map neg s) (f0:fs) (fc:afs) ( r - 1 )
                _ -> error "Could not find predecessor when finding MIC"
                   
-- | Finds a CTI given a safety property clause
nextCTI :: Frame -> Clause -> Model -> [Lit]
nextCTI frame prop m =
  case unsafePerformIO (increment ctiCount >> return res) of
    Just ls -> case pred ls of
                 Just ps -> getVarsFrom ps ls
                 _       -> error "Should be UNSAT."
    _       -> error "No CTI found."
  where
    res = unsafePerformIO (increment queryCount >> return (
            model $ solveWithAssumps (solver frame) (map (prime.neg) prop) ))
    pred psc = unsafePerformIO (increment queryCount >> return (
                 conflict $ solveWithAssumps (solver (getFrameWith [map prime prop] m)) psc ))
    getVarsFrom [] _ = []
    getVarsFrom (Var p:ps) ls = if Var p `elem` ls
                                  then Var p:getVarsFrom ps ls
                                  else Neg p:getVarsFrom ps ls
    getVarsFrom (Neg p:ps) ls = if Var p `elem` ls
                                  then Var p:getVarsFrom ps ls
                                  else Neg p:getVarsFrom ps ls
    getVarsFrom (p:ps) ls = getVarsFrom ps ls
   
removeSubsumed :: [Clause] -> [Clause]
removeSubsumed cs =
  removeSubsumed' (sortBy (comparing length) cs) []
  where
    removeSubsumed' (c:cs) acc =
      removeSubsumed' (reverse $ remove c cs []) (c:acc)
    removeSubsumed' _ acc = acc
    remove cls (c:cs) acc =
      if null (cls \\ c)
        then remove cls cs acc
        else remove cls cs (c:acc)
    remove _ _ acc = acc

-- | Push clauses to next frame
push :: Frame -> Model -> Frame -> (Frame, Bool, Frame)
push f model f' =
  pusher (cleaned \\ clauses f') True f'
  where
    cleaned = removeSubsumed (clauses f)
    newF = if length cleaned == length (clauses f)
             then f
             else getFrameWith cleaned model
    pusher (c:cs) b f' = 
      if consecution newF c
      then pusher cs b (addClauseToFrame f' c)
      else pusher cs False f'
    pusher _ b f' = (newF, b, addTransitionToFrame f' model)
