module IC3 ( prove ) where

import Model.Model
import Minisat.Minisat
import Data.Word

data Frame = Frame { solver  :: Solver
                   , clauses :: [Clause] }

addClauseToFrame f c = Frame { solver = addClause (solver f) c
                             , clauses = c:clauses f }

getFrame vars clauses = Frame { solver = addClauses (addVars newSolver vars) clauses
                              , clauses = clauses }

-- Keep transitions out of the clauses list
addTransitionToFrame f' model = Frame { solver = addClauses (solver f') (transition model)
                                      , clauses = clauses f' }

prove :: Model -> Lit -> [Frame] -> IO ()
prove model prop frames =
  case initiation model prop frames of
    Just frame -> do
                  res <- consecution model prop [frame]
                  if res then print "Proven." else print "Not proven."
    Nothing -> print "Cannot prove."

initiation :: Model -> Lit -> [Frame] -> Maybe Frame
initiation model prop frames =
  case frames of
    [] -> base (addTransitionToFrame (getFrame (vars model) $ initial model) model) prop
    fs -> base (fs !! (length fs - 1)) prop

consecution :: Model -> Lit -> [Frame] -> IO Bool
consecution model prop frames =
  case frames of
  (f:fs) -> if nextHas f [prop]
            then do
                 print $ "Frame " ++ show (length frames)
                 case push f model of
                   (True, _) -> return True
                   (False, f') -> consecution model prop (f':f:fs)
            else printVars (solver f) (vars model) >> error "Not handled yet."
              -- getCTI frame

getCTI :: Frame -> Word -> [Lit]
getCTI frame numVars = map neg $ getLiterals (solver frame) numVars

-- Push clauses to next frame
push :: Frame -> Model -> (Bool, Frame)
push f model =
  pusher (clauses f) True (getFrame (vars model) [])
  where
  pusher (c:cs) b f' = 
    if nextHas f c
    then pusher cs b (addClauseToFrame f' c)
    else pusher cs False f'
  pusher _ b f' = (b, addTransitionToFrame f' model)

-- I => P : Need I && (not P) to be UNSAT
base :: Frame -> Lit -> Maybe Frame
base f property =
  if solveWithAssumps (solver f) [neg property]
  then Nothing
  else Just newFrame
  where
  newFrame = addClauseToFrame f [property]

-- F_k && T => P' : Need F_k && T && (not P') to be UNSAT
nextHas :: Frame -> Clause -> Bool
nextHas f =
  next (solver f)
  where
  next solver (l:ls) = not (solveWithAssumps solver [prime $ neg l]) && next solver ls
  next solver [] = True 
