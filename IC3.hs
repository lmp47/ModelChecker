{- |
Module : IC3
Description : Model checking algorithm
-}

module IC3 ( prove ) where

import Model.Model
import Minisat.Minisat
import Data.Word
import System.IO.Unsafe
import Data.List hiding (insert)
import Data.IORef
import Debug.Trace
import Data.Ord
import Data.PQueue.Min hiding (map, drop, take, (!!))

-- Debug output / stats

ctiCount :: IORef Int
{-# NOINLINE ctiCount #-}
ctiCount = unsafePerformIO (newIORef 0)

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

type Obligation = (Int, Clause)

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
  unsafePerformIO (zero ctiCount >> zero queryCount >> return (
    initiation f0 [prop] && prove' model (singleton (1, [prop])) (addClauseToFrame f0 [prop])
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
stats :: [Frame] -> IORef Int -> IORef Int -> a -> a
stats frames cc qc = trace ("Number of frames: " ++ show (length frames) ++
                            "\nAverage number of literals/clause (not counting transition relation): "
                            ++ show(calcAvgLitsPerCls frames) ++
                            "\nNumber of ctis: " ++ (show $ unsafePerformIO $ readR cc) ++ 
                            "\nNumber of queries: " ++ (show $ unsafePerformIO $ readR qc) )

prove' :: Model -> MinQueue Obligation -> Frame -> Bool
prove' m queue frame =
  case proveObligations m queue [frame] of
  (res, frames, _) -> stats frames ctiCount queryCount res

proveObligations :: Model -> MinQueue Obligation -> [Frame] -> (Bool, [Frame], MinQueue Obligation)
proveObligations m queue frames =
  if consecution frame prop
    then pushFrame frame nextFrame
    else
      let negCTI = (map neg $ fst $ currentNext $ nextCTI frame prop m) in
        case initiation (head frames) negCTI of
          True -> case propagate (addClauseToFrame (head frames) negCTI:tail frames) 1 of
                    Just (frames', d) -> proveObligations m (insert (d, negCTI) queue) frames'
                    Nothing -> (True, frames, queue)
          _ -> (False, frames, queue)
  where
    ((depth, prop), queue') = deleteFindMin queue
    pre = take (depth - 1) frames
    frame = frames !! (depth - 1)
    nextFrame = if depth == length frames then getFrameWith [] m else frames !! depth
    post = if depth < length frames then drop (depth + 1) frames else []
    -- Push all possible clauses from frame f to frame f'
    pushFrame f f' =
      case push f m f' of
        (_, True, _) -> (True, frames, queue)
        (f, False, f'') -> proveObligations m (insert (depth + 1, prop) queue') (pre ++ f:f'':post)
    -- Push all clauses as far as possible until the current obligation depth
    -- and see if fixed point has been reached (resulting in Nothing)
    propagate (f:f':frames) d
      | d < depth - 1 =
        case push f m f' of
          (_, True, f'') -> Nothing
          (f, False, f'') -> case propagate (f'':frames) (d + 1) of
                              Just (fs, d) -> Just ((f:fs), d)
                              Nothing -> Nothing
      | otherwise = Just ((f:f':frames), d)
    propagate frames d = Just (frames, d)

-- | Find an approximate minimal subclause of the provided clause that satisfies initiation
-- and consecution.
inductiveGeneralization :: Clause -> Frame -> Frame -> Model -> Word -> Clause
inductiveGeneralization clause f0 fk m = generalize clause f0 fk []
  where
    generalize cs _ _ needed 0 = cs ++ needed
    generalize [] _ _ needed _ = needed
    generalize (c:cs) f0 fk needed k =
      let res = unsafePerformIO (increment queryCount >> return (
                  solveWithAssumps (solver (getFrameWith (cs:(clauses fk)) m)) (map (prime.neg) cs)
                )) in
        if not (satisfiable res) && initiation f0 cs
          then generalize cs f0 fk needed k
          else generalize cs f0 fk (c:needed) ( k - 1 )

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
                 conflict $ solveWithAssumps (solver (getFrameWith [map prime prop] m)) (psc) ))
    getVarsFrom [] _ = []
    getVarsFrom (Var p:ps) ls = if Var p `elem` ls
                                  then Var p:(getVarsFrom ps ls)
                                  else Neg p:(getVarsFrom ps ls)
    getVarsFrom (Neg p:ps) ls = if Var p `elem` ls
                                  then Var p:(getVarsFrom ps ls)
                                  else Neg p:(getVarsFrom ps ls)
    getVarsFrom (p:ps) ls = getVarsFrom ps ls
   
removeSubsumed :: [Clause] -> [Clause]
removeSubsumed cs =
  removeSubsumed' (sortBy (comparing length) cs) []
  where
    removeSubsumed' (c:cs) acc =
      removeSubsumed' (reverse $ remove c cs []) (c:acc)
    removeSubsumed' _ acc = acc
    remove cls (c:cs) acc =
      if (cls \\ c) == []
        then remove cls cs acc
        else remove cls cs (c:acc)
    remove _ _ acc = acc

-- | Push clauses to next frame
push :: Frame -> Model -> Frame -> (Frame, Bool, Frame)
push f model f' =
  pusher (cleaned \\ clauses f') True f'
  where
    cleaned = removeSubsumed (clauses f)
    newF = if (length (cleaned) == length (clauses f))
             then f
             else getFrameWith cleaned model
    pusher (c:cs) b f' = 
      if consecution newF c
      then pusher cs b (addClauseToFrame f' c)
      else pusher cs False f'
    pusher _ b f' = (newF, b, addTransitionToFrame f' model)
