{- |
Module : IC3
Description : Model checking algorithm
-}

module IC3 ( prove ) where

import Model.Model
import Minisat.Minisat
import Data.Word
import System.IO.Unsafe
import Data.List
import System.IO
import Data.IORef
import Debug.Trace

ctiCount :: IORef Int 
{-# NOINLINE ctiCount #-}
ctiCount = unsafePerformIO (newIORef 0)

queryCount :: IORef Int 
{-# NOINLINE queryCount #-}
queryCount = unsafePerformIO (newIORef 0)

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
initiation f prop = unsafePerformIO (modifyIORef' queryCount (+ 1) >> return (
                      not (satisfiable (solveWithAssumps (solver f) (map neg prop))) ))

-- | Consecution query. Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals.
consecution :: Frame -> Clause -> Bool
consecution f prop = unsafePerformIO (modifyIORef' queryCount (+ 1) >> return (
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
stats :: [Frame] -> a -> a
stats frames = trace ("Number of frames: " ++ show (length frames) ++
                      "\nAverage number of literals/clause (not counting transition relation): "
                      ++ show(calcAvgLitsPerCls frames) ++
                      "\nNumber of ctis: " ++ (show $ unsafePerformIO $ readIORef ctiCount) ++
                      "\nNumber of queries: " ++ (show $ unsafePerformIO $ readIORef queryCount) )

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
                                    Nothing -> stats (frame:acc) True)
          (False, frame', acc') -> stats (frame:acc) False
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

-- | Try to prove CTI unreachable at current depth given the current frame, CTI, previous frames and property
proveNegCTI :: Model -> Frame -> [Lit] -> [Frame] -> Clause -> (Bool, Frame, [Frame])
proveNegCTI m f _ [] p = (False, f, [])
proveNegCTI m f cti acc p =
    if satisfiable (solveWithAssumps (solver (getFrameWith (map neg cti:clauses (head acc)) m)) (map prime cti))
      then (False, f, acc)
      else
        case pushCTI (map neg cti) acc f of
          (Nothing, acc', f', []) -> if consecution f' p
                                       then (True, f', acc')
                                       else proveNegCTI m f' (fst (currentNext (nextCTI f' p m))) acc' p
          (Just model, acc', f', fs) -> case proveNegCTI m f' (fst (currentNext model)) acc' (map neg cti) of
                                          (True, f'', acc'') -> proveNegCTI m f cti
                                                                  (acc'' ++ f'':take (length fs - 1) fs) p
                                          false              -> false
  where
    -- Find the deepest frame where the negated CTI holds
    pushCTI negCTI [] f = (Nothing, [], f, [])
    pushCTI negCTI acc f =
      let res = solveWithAssumps
                  (solver (getFrameWith (negCTI:clauses (acc !! (length acc - 1))) m))
                  (map (prime.neg) negCTI) in
        if not (satisfiable res)
          then let negCTI' = inductiveGeneralization negCTI (head acc) f in
            (Nothing, map (`addClauseToFrame` negCTI') acc, addClauseToFrame f negCTI', [])
          else
            case pushCTI negCTI (take (length acc - 1) acc) (acc !! (length acc - 1)) of
              (Just model, acc', f', leftover)  -> (Just model, acc', f', leftover ++ [f])
              (Nothing, acc', f', []) -> (Just (nextCTI (acc !! (length acc - 1)) negCTI m), acc', f', [f])

-- | Find a minimal subclause of the provided clause that satisfies initiation and
-- consecution.
inductiveGeneralization :: Clause -> Frame -> Frame -> Clause
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
  case unsafePerformIO (modifyIORef' ctiCount (+ 1) >> return res) of
    Just ls -> ls
    _       -> error "No CTI found."
  where
    res = unsafePerformIO (modifyIORef' queryCount (+ 1) >> return (
            model $ solveWithAssumps (solver (getFrameWith (clauses frame) m)) (map (prime.neg) prop)) )

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
