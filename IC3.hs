module IC3 ( prove ) where

import Model.Model
import Minisat.Minisat

data Frame = Frame { solver  :: IO Solver
                   , clauses :: [Clause] }

addClauseToFrame f c = Frame { solver = do
                                        solver <- solver f
                                        addClause solver c
                                        return solver
                             , clauses = c:(clauses f) }

getFrame vars clauses = Frame { solver = do
                                         solver <- newSolver
                                         addVars solver vars
                                         addClauses solver clauses
                                         return solver
                         , clauses = clauses }

-- Keep transitions out of the clauses list
addTransitionToFrame f' model = Frame { solver = do
                                           solver <- solver f'
                                           addClauses solver $ transition model
                                           return solver
                                , clauses = (clauses f') }

prove model prop frames =
  do
  init <- initiation model prop frames
  case init of
    Just frame -> do
                  res <- consecution model prop [frame]
                  if res then print "Proven." else print "Not proven."
    Nothing -> print "Cannot prove."

initiation :: Model -> Lit -> [Frame] -> IO (Maybe Frame)
initiation model prop frames =
  case frames of
    [] -> base (addTransitionToFrame (getFrame (vars model) $ initial model) model)  prop model
    fs -> base (fs !! ((length fs) - 1)) prop model

consecution :: Model -> Lit -> [Frame] -> IO Bool
consecution model prop frames =
  case frames of
  (f:fs) -> do
            next <- (nextHas f [prop])
            if next
            then do
                 pushres <- push f model
                 print $ "Frame " ++ (show $ length frames)
                 case pushres of
                   (True, _) -> return True
                   (False, f') -> consecution model prop (f':f:fs)
            else error "Not handled yet."

-- Push clauses to next frame
push :: Frame -> Model -> IO (Bool, Frame)
push f model =
  pusher (clauses f) True (getFrame (vars model) [])
  where
  pusher (c:cs) b f' = 
    do
    next <-  nextHas f c
    if next
    then pusher cs b (addClauseToFrame f' c)
    else pusher cs False f'
  pusher _ b f' = return (b, addTransitionToFrame f' model)

-- I => P : Need I && (not P) to be UNSAT
base :: Frame -> Lit -> Model -> IO (Maybe Frame)
base f property model =
  do
  solver <- solver f
  res <- (solveWithAssumps solver $ [neg property])
  if res
  then do
       printVars solver (vars model)
       return Nothing
  else return $ Just newFrame
  where
  newFrame = addClauseToFrame f [property]

-- F_k && T => P' : Need F_k && T && (not P') to be UNSAT
nextHas :: Frame -> Clause -> IO Bool
nextHas f property =
  do
  solver <- (solver f)
  next solver property
  where
  next solver (l:ls) = do
                       res <- solveWithAssumps solver [prime $ neg l]
                       if res then return False else (next solver ls)
  next solver [] = return True 
