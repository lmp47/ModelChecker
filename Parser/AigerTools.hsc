{-|
Module: AigerTools
Description: Alternate AIGER format parser

Haskell bindings for a very small subset of @aiger.c@
(just enough so that one of the functions to read AIGER-formatted
files can be used)
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module AigerTools (getModelFromFile) where

import AigModel (litFromAiger, Lit, Latch, And)
import qualified AigModel as Model
import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Environment

#include "../aiger/aiger.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Accepts a filepath for an AIGER (@aig@ or @aag@) file and returns the
-- corresponding 'AigModel.Model'
getModelFromFile :: String -> IO Model.Model
getModelFromFile s = do
                     aiger <- readAigerFromFile s
                     return $ getModel aiger

getModel :: Aiger -> Model.Model
getModel aiger = Model.Model { Model.inputs      = map getLit $ inputs aiger
                             , Model.latches     = map getLatch $ latches aiger
                             , Model.outputs     = map getLit $ outputs aiger
                             , Model.ands        = map getAnd $ ands aiger
                             , Model.bad         = map getLit $ bad aiger
                             , Model.constraints = map getLit $ constraints aiger
                             }

getLit :: AigerLit -> Lit
getLit (AigerLit lit) = litFromAiger $ fromIntegral lit

getLatch :: AigerLatch -> Latch
getLatch latch = [ litFromAiger $ fromIntegral $ lit latch
                 , litFromAiger $ fromIntegral $ next latch
                 , litFromAiger $ fromIntegral $ reset latch
                 ]

getAnd :: AigerAnd -> And
getAnd and = [ litFromAiger $ fromIntegral $ lhs and
             , litFromAiger $ fromIntegral $ rhs0 and
             , litFromAiger $ fromIntegral $ rhs1 and
             ]

------------------------
-- Storable Datatypes --
------------------------

-- The aiger_model struct datatype
-- (with justice and fairness properties ignored)
data Aiger = Aiger { inputs      :: [AigerLit]
                   , latches     :: [AigerLatch]
                   , outputs     :: [AigerLit]
                   , ands        :: [AigerAnd]
                   , bad         :: [AigerLit]
                   , constraints :: [AigerLit]
                   }

instance Storable Aiger where

  alignment _ = #{alignment aiger} --alignment (undefined :: CInt)

  sizeOf _ = #{size aiger}

  peek ptr = do
             numInputs <- #{peek aiger, num_inputs} ptr
             numLatches <- #{peek aiger, num_latches} ptr
             numOutputs <- #{peek aiger, num_outputs} ptr
             numAnds <- #{peek aiger, num_ands} ptr
             numBad <- #{peek aiger, num_bad} ptr
             numConstraints <- #{peek aiger, num_constraints} ptr
             return Aiger
               `ap` ((#{peek aiger, inputs} ptr) >>= peekArray (toInt numInputs))
               `ap` ((#{peek aiger, latches} ptr) >>= peekArray (toInt numLatches))
               `ap` ((#{peek aiger, outputs} ptr) >>= peekArray (toInt numOutputs))
               `ap` ((#{peek aiger, ands} ptr) >>= peekArray (toInt numAnds))
               `ap` ((#{peek aiger, bad} ptr) >>= peekArray (toInt numBad))
               `ap` ((#{peek aiger, constraints} ptr) >>= peekArray (toInt numConstraints))
             where
               toInt :: CUInt -> Int
               toInt = fromIntegral

  -- Do not want to pass any information back into C
  poke _ _ = return ()

-- The aiger_symbol struct is given two corresponding
-- datatypes: AigerLit and AigerLatch

-- AigerLit
data AigerLit = AigerLit CUInt

instance Storable AigerLit where

  alignment _ = #{alignment aiger_symbol}

  sizeOf _ = #{size aiger_symbol}

  peek ptr = return AigerLit `ap` (#{peek aiger_symbol, lit} ptr)

  poke ptr (AigerLit int) = do
    #{poke aiger_symbol, lit} ptr int

-- AigerLatch
data AigerLatch = AigerLatch { lit   :: CUInt
                             , next  :: CUInt
                             , reset :: CUInt
                             }

instance Storable AigerLatch where

  alignment _ = #{alignment aiger_symbol}

  sizeOf _ = #{size aiger_symbol}

  peek ptr = return AigerLatch
             `ap` (#{peek aiger_symbol, lit} ptr)
             `ap` (#{peek aiger_symbol, next} ptr)
             `ap` (#{peek aiger_symbol, reset} ptr)
  
  poke ptr latch = do
    #{poke aiger_symbol, lit} ptr $ lit(latch)
    #{poke aiger_symbol, next} ptr $ next(latch)
    #{poke aiger_symbol, reset} ptr $ reset(latch)

-- The aiger_and struct datatype
data AigerAnd = AigerAnd { lhs  :: CUInt
                         , rhs0 :: CUInt
                         , rhs1 :: CUInt
                         }

instance Storable AigerAnd where

  alignment _ = #{alignment aiger_and}
  
  sizeOf _ = #{size aiger_and}

  peek ptr = return AigerAnd
             `ap` (#{peek aiger_and, lhs} ptr)
             `ap` (#{peek aiger_and, rhs0} ptr)
             `ap` (#{peek aiger_and, rhs1} ptr)

  poke ptr and = do
    #{poke aiger_and, lhs} ptr $ lhs(and)
    #{poke aiger_and, rhs0} ptr $ rhs0(and)
    #{poke aiger_and, rhs1} ptr $ rhs1(and)

-----------------
-- C Functions --
-----------------

foreign import ccall unsafe "../aiger/aiger.h aiger_init"
  aigerInit :: IO (Ptr Aiger)

foreign import ccall unsafe "../aiger/aiger.h aiger_reset"
  aigerReset :: Ptr Aiger -> IO ()

foreign import ccall unsafe "../aiger/aiger.h aiger_open_and_read_from_file"
  readFromFile :: Ptr Aiger -> CString -> IO (CString)

readAigerFromFile :: String -> IO Aiger
readAigerFromFile fileName = 
  do
  aigerPtr <- aigerInit
  cFileName <- newCString fileName
  err <- readFromFile aigerPtr cFileName
  if (err /= nullPtr)
    then do
         msg <- peekCString err
         error $ "Problem parsing file. " ++ msg
    else do
      aiger <- peek aigerPtr
      aigerReset aigerPtr
      return aiger
