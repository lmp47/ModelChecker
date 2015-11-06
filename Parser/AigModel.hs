{-|
Module: Parser.AigModel
Description: A representation of AIG models

A representation of AIG models.
-}

---------------
-- AIG Model --
---------------

module Parser.AigModel ( Lit(Var, Neg, Boolean)
                       , Latch
                       , And
                       , Model( Model
                              , numVars
                              , numInputs
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
data Lit = Var Word | Neg Word | Boolean Bool deriving (Show, Eq, Ord)

-- | A representation of a latch
type Latch = [Lit]

-- | A representation of an AND gate
type And = [Lit]

-- | A representation of an AIG model
data Model = Model { numVars     :: Word
                   , numInputs   :: Word
                   , latches     :: [Latch]
                   , outputs     :: [Lit]
                   , ands        :: [And]
                   , bad         :: [Lit]
                   , constraints :: [Lit]
                   } deriving Show

-- | Makes literals from AIGER literals
litFromAiger :: Word -> Lit
litFromAiger 0   = Boolean False
litFromAiger 1   = Boolean True
litFromAiger int =
  case int `rem` 2 of
  0 -> Var $ (int `div` 2) - 1
  _ -> Neg $ (int `div` 2) - 1
