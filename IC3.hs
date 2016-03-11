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

readR :: IORef Int -> IO (Int)
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
  (sum (map calcAvgPerFrame frames)) / (fromIntegral (length frames))
  where
  calcAvgPerFrame f =
    let cls = clauses f in
      (fromIntegral (sum (map length cls))) / (fromIntegral (length cls))

-- | Trace stat output
stats :: [Frame] -> IORef Int -> IORef Int -> IORef Int -> a -> a
stats frames cc gc qc = trace ("Number of frames: " ++ show (length frames) ++
                            "\nAverage number of literals/clause (not counting transition relation): "
                            ++ show(calcAvgLitsPerCls frames) ++
                            "\nNumber of ctis: " ++ (show $ unsafePerformIO $ readR cc) ++ 
                            "\nNumber of ctgs: " ++ (show $ unsafePerformIO $ readR gc) ++ 
                            "\nNumber of queries: " ++ (show $ unsafePerformIO $ readR qc) )

-- | Consecution phase of the algorithm (calls subsequent consecution queries for
-- other frames)
prove' :: Model -> Lit -> Frame -> [Frame] -> Bool
prove' m prop frame acc =
  if consecution frame [prop]
    then pushFrame frame (getFrame (vars m) []) acc
    else
      let cti = nextCTI frame [prop] m in
        case proveNegCTI m frame (fst $ currentNext cti) acc [prop] of
          (True, frame', acc') -> (clauses frame' /= clauses frame) &&
                                  (case propagate (acc' ++ [frame']) of
                                    Just fs ->
                                      prove' m prop (fs !! (length fs - 1))
                                        (take (length fs - 1) fs)
                                    Nothing -> stats (frame:acc) ctiCount ctgCount queryCount True)
          (False, frame', acc') -> stats (frame:acc) ctiCount ctgCount queryCount False
  where
    -- Push all possible clauses from frame f to frame f'
    pushFrame f f' acc =
      case push f m f' of
        (True, _) -> stats (frame:acc) ctiCount ctgCount queryCount True
        (False, f'') -> prove' m prop f'' (acc ++ [f])
    -- Push all clauses and see if fixed point has been reached (resulting in Nothing)
    propagate (f:f':frames) =
      case push f m f' of
        (True, f'') -> Nothing
        (False, f'') -> case propagate (f'':frames) of
                          Just fs -> Just (f:fs)
                          Nothing -> Nothing
    propagate frames = Just frames

-- | Try to prove CTI unreachable at current depth given the current frame, CTI, previous frames and property
proveNegCTI :: Model -> Frame -> [Lit] -> [Frame] -> Clause -> (Bool, Frame, [Frame])
proveNegCTI m f _ [] p = (False, f, [])
proveNegCTI m f cti acc p =
    if unsafePerformIO (increment queryCount >> return (
         satisfiable (solveWithAssumps (solver (getFrameWith (map neg cti:clauses (head acc)) m)) (map prime cti))
       ))
      then (False, f, acc)
      else
        case pushNegCTI (map neg cti) acc f of
          (Nothing, acc', f') -> if consecution f' p
                                   then (True, f', acc')
                                   else proveNegCTI m f' (fst (currentNext (nextCTI f' p m))) acc' p
          (Just model, acc', f') -> case proveNegCTI m f' (fst (currentNext model)) acc' (map neg cti) of
                                      (True, f'', acc'') -> proveNegCTI m f cti (acc'' ++ [f'']) p
                                      false              -> false
  where
    pushNegCTI negCTI [] f = (Nothing, [], f)
    pushNegCTI negCTI acc f =
      let res = unsafePerformIO ( increment queryCount >> return (
                  solveWithAssumps
                  (solver (getFrameWith (negCTI:clauses (acc !! (length acc - 1))) m))
                  (map (prime.neg) negCTI) )) in
        if not (satisfiable res)
          then let (negCTI', fs) = inductiveGeneralization negCTI acc f m 3 3 in
            (Nothing, map (`addClauseToFrame` negCTI') (take (length fs - 1) fs), addClauseToFrame (last fs) negCTI')
          else let f' = acc !! (length acc - 1) in
            (Just (nextCTI f' negCTI m), take (length acc - 1) acc, f')

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
inductiveGeneralization :: Clause -> [Frame] -> Frame -> Model -> Word -> Word -> (Clause, [Frame])
inductiveGeneralization clause fs fk m w w' = generalize clause fs fk [] w w'
  where
    generalize cs fs fk needed 0 _ = (cs ++ needed, fs ++ [fk])
    generalize [] fs fk needed _ _ = (needed, fs ++ [fk])
    generalize (l:ls) fs fk needed k r =
      case down ls fs [fk] r of
        Just (fs', [fk'], ls') -> generalize ls' fs' fk' needed k r
        Nothing -> generalize ls fs fk (l:needed) ( k - 1 ) r
    down ls (f0:fs) (fk:fl) r =
      -- Check initiation and consecution for the potential generalization
      let init = initiation f0 ls
          consec = unsafePerformIO (increment queryCount >> return (
                     solveWithAssumps (solver (getFrameWith (ls:(clauses fk)) m)) (map (prime.neg) ls)
                   )) in
      if not(init) || r == 0
        then Nothing
        else
          if not (satisfiable consec)
            then Just (f0:fs, fk:fl, ls) -- Generalization succeeded
            else
              case model consec of
                Just s ->  -- Try to push negCTG as far as possible
                  let negCTG = map neg (fst (currentNext s)) in
                    if not(null fs) && initiation f0 negCTG && consecution (last fs) negCTG
                      then unsafePerformIO (increment ctgCount >> return (
                        let rest = take (length fl) (fk:fl)
                            lastf = last (fk:fl)
                            (ctgs, nctgs) = pushNegCTG negCTG [] rest m
                            ctgs' = f0:fs ++ ctgs
                            (fdps, fd) = (take (length ctgs' - 1) ctgs', last ctgs') -- fd is deepest frame with negCTI inductive
                            (c, fs') = generalize negCTG fdps fd [] w ( r - 1 ) in
                              down ls fs' (map (`addClauseToFrame` c) (nctgs ++ [lastf]) ++ tail (nctgs ++ [lastf])) r ))
                      else down (ls `intersect` (map neg s)) (f0:fs) (fk:fl) ( r - 1 )
                _ -> error "Could not find predecessor when finding MIC"
                   

-- | Finds a CTI given a safety property clause
nextCTI :: Frame -> Clause -> Model -> [Lit]
nextCTI frame prop m =
  case unsafePerformIO (increment ctiCount >> return res) of
    Just ls -> ls
    _       -> error "No CTI found."
  where
    res = unsafePerformIO (increment queryCount >> return (
            model $ solveWithAssumps (solver (getFrameWith (clauses frame) m)) (map (prime.neg) prop)) )

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
