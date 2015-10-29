{-|
Module: AigModel
Description: A representation of AIG models

A representation of AIG models
-}

---------------
-- AIG Model --
---------------

module AigModel ( Lit(Var, Neg, Boolean)
                , Latch
                , And
                , Model( Model
                       , inputs
                       , latches
                       , outputs
                       , ands
                       , bad
                       , constraints
                       )
                , litFromAiger
                ) where

import Data.Word
import Data.List

-- | A boolean literal
data Lit = Var Word | Neg Word | Boolean Bool deriving (Show, Eq)
-- | A representation of a latch
type Latch = [Lit]
-- | A representation of an AND gate
type And = [Lit]
-- | A representation of an AIG model
data Model = Model { inputs      :: [Lit]
                   , latches     :: [Latch]
                   , outputs     :: [Lit]
                   , ands        :: [And]
                   , bad         :: [Lit]
                   , constraints :: [Lit]
                   } deriving Show

instance Eq Model where
  (==) m1 m2 = ((inputs m1) \\ (inputs m2) == []) &&
               ((latches m1) \\ (latches m2) == []) &&
               ((outputs m1) \\ (outputs m2) == []) &&
               ((ands m1) \\ (ands m2) == []) &&
               ((bad m1) \\ (bad m2) == []) &&
               ((constraints m1) \\ (constraints m2) == [])
 
-- | Makes literals from AIGER literals
litFromAiger :: Word -> Lit
litFromAiger 0   = Boolean False
litFromAiger 1   = Boolean True
litFromAiger int =
  case int `rem` 2 of
  0 -> Var $ (int `div` 2) - 1
  _ -> Neg $ (int `div` 2) - 1
