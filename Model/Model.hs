{- |
Module : Model
Description : Model for representing transition system and safety property
-}

module Model.Model ( Model (vars, initial, transition, safe)
                   , Lit(Var, Neg, Var', Neg')
                   , Clause
                   , toModel
                   , prime
                   , neg
                   , currentNext ) where

import qualified Parser.AigModel as Aig
import Data.Word

-- | A transition system with safety property type. Contains number
-- of variables, initial states, transition relation and safety property
data Model = Model { vars :: Word
                   , initial :: [Clause]
                   , transition :: [Clause]
                   , safe :: Lit } deriving Show
data Lit = Var Word | Neg Word | Var' Word | Neg' Word deriving (Show, Eq)
type Clause = [Lit]


toLit :: Aig.Lit -> Lit
toLit (Aig.Var l) = Var l
toLit (Aig.Neg l) = Neg l
toLit _           = error "Cannot get Lit from Boolean"

toNegLit :: Aig.Lit -> Lit
toNegLit = neg.toLit

-- | Negate a literal
neg :: Lit -> Lit
neg (Var l)  = Neg  l
neg (Neg l)  = Var  l
neg (Var' l) = Neg' l
neg (Neg' l) = Var' l

-- | Convert an unprimed literal into a primed one
prime :: Lit -> Lit
prime (Var l) = Var' l
prime (Neg l) = Neg' l
prime lit   = error ("Cannot prime a prime: " ++ show lit)

-- | Split a list of literals into unprimed and primed lists of literals
currentNext :: [Lit] -> ([Lit], [Lit])
currentNext ls = cn ls [] []
  where
  cn [] curr next = (curr, next)
  cn (l:ls) curr next =
    case l of
    Var _ -> cn ls (l:curr) next
    Neg _ -> cn ls (l:curr) next
    _     -> cn ls curr (l:next)

-- | Turn 'Parser.AigModel.Model's into transition systems with a safety property
toModel :: Aig.Model -> Model
toModel m =
  Model { vars = vars
        , initial =  initial m
        , transition = transition m
        , safe = neg $ toLit prop}
  where
  vars = 2 * Aig.numVars m
  ins' = makePrimeInputs $ Aig.numInputs m
  (latches, latches') = makeLatches $ Aig.latches m
  gates  = makeAnds $ Aig.ands m
  gates' = map (map prime) gates
  initial m =
    latches ++ map (\x -> [toLit x]) (Aig.constraints m)
  transition m =
    ins' ++ latches' ++ gates' ++ gates
  badprops = Aig.bad m
  outprops = Aig.outputs m
  prop | not (null badprops) = head badprops
       | not (null outprops) = head outprops
       | otherwise = error "No property to prove."

-- Inputs
-- in <==> in'
makePrimeInputs :: Word -> [Clause]
makePrimeInputs numIns =
  if numIns > 0 then foldl1 (++) inputs' else [] 
  where
  inputs' = [[[Var' input, Neg input], [Neg' input, Var input]] | input <- [0..(numIns - 1)]]

-- Latches
-- next <=> latch' = (next ==> latch') and (latch' ==> next)
-- next ==> latch' = not(next) or latch'
-- latch' ==> next = not(latch') or next
makeLatches :: [Aig.Latch] -> ([Clause], [Clause])
makeLatches ([Aig.Var latch, next, init]:ls) =
  case next of
  Aig.Boolean True  -> (inits, [Var' latch]:primes)
  Aig.Boolean False -> (inits, [Neg' latch]:primes)
  _ -> (inits, [Var' latch, toNegLit next]:[Neg' latch, toLit next]:primes)
  where
  rest = makeLatches ls
  initRest = fst rest
  primes = snd rest
  inits = case init of
          Aig.Boolean True  -> [Var latch]:initRest
          Aig.Boolean False -> [Neg latch]:initRest
          _ -> initRest
makeLatches [] = ([],[])

-- Ands
-- gate <=> in1 and in2 = (gate ==> in1 and in2) and (in1 and in2 ==> gate)
-- gate ==> in1 and in2 = not(gate) or (in1 and in2) = (not(gate) or in1) and (not(gate) or in2)
-- in1 and in2 ==> gate = not(in1 and in2) or gate = not(in1) or not(in2) or gate
makeAnds :: [Aig.And] -> [Clause]
makeAnds ([Aig.Var gate, in1, in2]:as) = and ++ rest
  where
  and = case (in1, in2) of
        (Aig.Boolean b1, Aig.Boolean b2) -> if b1 && b2 then [[Var gate]] else [[Neg gate]]
        (Aig.Boolean b1, _) -> if b1 then [[Neg gate, toLit in2], [toNegLit in2, Var gate]] else [[Neg gate]]
        (_, Aig.Boolean b2) -> if b2 then [[Neg gate, toLit in1], [toNegLit in1, Var gate]] else [[Neg gate]]
        _ -> [[Neg gate, toLit in1], [Neg gate, toLit in2], [toNegLit in1, toNegLit in2, Var gate]]
  rest = makeAnds as
makeAnds [] = []
