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
import Data.Ord
import Data.PQueue.Min hiding (map, drop, take, (!!), null)

type Obligation = (Int, Int, Clause)

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
  initiation f0 [prop] && proveObligations model (singleton (1, 0, [prop])) [addClauseToFrame f0 [prop]] 1
  where
    f0 = getFrameWith (initial model) model

-- | Initiation query. Checks if I && (not P) is UNSAT.
initiation :: Frame -> Clause -> Bool
initiation f prop = not (satisfiable (solveWithAssumps (solver f) (map neg prop)))

-- | Consecution query. Checks if F_k && T && (not P') is UNSAT, where P is a disjunction of literals.
consecution :: Frame -> Clause -> Bool
consecution f prop = not (satisfiable (solveWithAssumps (solver f) (map (prime.neg) prop)))

proveObligations :: Model -> MinQueue Obligation -> [Frame] -> Int -> Bool
proveObligations m queue frames rank =
  if consecution frame prop
    then pushFrame frame nextFrame
    else
      let negCTI = (map neg $ fst $ currentNext $ nextCTI frame prop m) in
        if initiation (head frames) negCTI
          then case propagate (head frames) (addClauseToFrame (head frames) negCTI:tail frames) negCTI 1 of
                 Just (frames', d) -> proveObligations m
                                        (insert (d, rank, inductiveGeneralization negCTI (head frames) (frames !! (d - 1)) m 3) queue)
                                        frames' (rank + 1)
                 Nothing -> True
          else False
  where
    ((depth, r, prop), queue') = deleteFindMin queue
    pre = take (depth - 1) frames
    frame = frames !! (depth - 1)
    nextFrame = if depth == length frames then getFrameWith [] m else frames !! depth
    post = if depth < length frames then drop (depth + 1) frames else []
    -- Push all possible clauses from frame f to frame f'
    pushFrame f f' =
      case push (head frames) f m f' of
        (_, True, _) -> True
        (f, False, f'') -> proveObligations m (insert (depth + 1, r, prop) queue') (pre ++ f:f'':post) rank
    -- Push all clauses as far as possible until the current obligation depth
    -- and see if fixed point has been reached (resulting in Nothing)
    -- Need to check that negCTI is inside
    propagate f0 (f:f':frames) negCTI d
      | d < depth =
        case push f0 f m f' of
          (_, True, f'') -> Nothing
          (f, False, f'') -> if checkSubsumed negCTI (clauses f'')
                               then case propagate f0 (f'':frames) negCTI (d + 1) of
                                 Just (fs, d) -> Just (f:fs, d)
                                 Nothing -> Nothing
                               else Just (f:f'':frames, d)
      | otherwise = Just (f:f':frames, d)
    propagate _ frames _ d = Just (frames, d)
    checkSubsumed p (c:cs) = null (c \\ p) || checkSubsumed p cs
    checkSubsumed _ [] = False

-- | Find an approximate minimal subclause of the provided clause that satisfies initiation
-- and consecution.
inductiveGeneralization :: Clause -> Frame -> Frame -> Model -> Word -> Clause
inductiveGeneralization clause f0 fk m = generalize clause f0 fk []
  where
    generalize cs _ _ needed 0 = cs ++ needed
    generalize [] _ _ needed _ = needed
    generalize (c:cs) f0 fk needed k =
      let res = solveWithAssumps (solver (getFrameWith (cs:clauses fk) m)) (map (prime.neg) cs) in
        if not (satisfiable res) && initiation f0 cs
          then generalize cs f0 fk needed k
          else generalize cs f0 fk (c:needed) ( k - 1 )

-- | Finds a CTI given a safety property clause
nextCTI :: Frame -> Clause -> Model -> [Lit]
nextCTI frame prop m =
  case res of
    Just ls -> case pred ls of
                 Just ps -> getVarsFrom ps ls
                 _       -> error "Should be UNSAT."
    _       -> error "No CTI found."
  where
    res = model $ solveWithAssumps (solver frame) (map (prime.neg) prop)
    pred psc = conflict $ solveWithAssumps (solver (getFrameWith [map prime prop] m)) psc
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
push :: Frame -> Frame -> Model -> Frame -> (Frame, Bool, Frame)
push f0 f model f' =
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
