{-# LINE 1 "AigerTools.hsc" #-}
{-|
{-# LINE 2 "AigerTools.hsc" #-}
Module: Parser.AigerTools
Description: Alternate AIGER format parser

Haskell bindings for a very small subset of @aiger.c@
(just enough so that one of the functions to read AIGER-formatted
files can be used)
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Parser.AigerTools (getModelFromFile) where

import Parser.AigModel (litFromAiger, Lit, Latch, And)
import qualified Parser.AigModel as Model
import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Environment


{-# LINE 23 "AigerTools.hsc" #-}

{-# LINE 24 "AigerTools.hsc" #-}

-- | Accepts a filepath for an AIGER (@aig@ or @aag@) file and returns the
-- corresponding 'AigModel.Model'
getModelFromFile :: String -> IO Model.Model
getModelFromFile s = do
                     aiger <- readAigerFromFile s
                     return $ getModel aiger

getModel :: Aiger -> Model.Model
getModel aiger = Model.Model { Model.numVars     = numVars aiger
                             , Model.numInputs   = numInputs aiger
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
data Aiger = Aiger { numVars     :: Word
                   , numInputs   :: Word
                   , latches     :: [AigerLatch]
                   , outputs     :: [AigerLit]
                   , ands        :: [AigerAnd]
                   , bad         :: [AigerLit]
                   , constraints :: [AigerLit]
                   }

instance Storable Aiger where

  alignment _ = 8 --alignment (undefined :: CInt)
{-# LINE 75 "AigerTools.hsc" #-}

  sizeOf _ = (112)
{-# LINE 77 "AigerTools.hsc" #-}

  peek ptr = do
             numInputs <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 80 "AigerTools.hsc" #-}
             numLatches <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 81 "AigerTools.hsc" #-}
             numOutputs <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 82 "AigerTools.hsc" #-}
             numAnds <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 83 "AigerTools.hsc" #-}
             numBad <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 84 "AigerTools.hsc" #-}
             numConstraints <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 85 "AigerTools.hsc" #-}
             return Aiger
               `ap` (return (fromIntegral $ numInputs + numLatches + numAnds))
               `ap` (return (fromIntegral $ numInputs))
               `ap` (((\hsc_ptr -> peekByteOff hsc_ptr 48) ptr) >>= peekArray (toInt numLatches))
{-# LINE 89 "AigerTools.hsc" #-}
               `ap` (((\hsc_ptr -> peekByteOff hsc_ptr 56) ptr) >>= peekArray (toInt numOutputs))
{-# LINE 90 "AigerTools.hsc" #-}
               `ap` (((\hsc_ptr -> peekByteOff hsc_ptr 96) ptr) >>= peekArray (toInt numAnds))
{-# LINE 91 "AigerTools.hsc" #-}
               `ap` (((\hsc_ptr -> peekByteOff hsc_ptr 64) ptr) >>= peekArray (toInt numBad))
{-# LINE 92 "AigerTools.hsc" #-}
               `ap` (((\hsc_ptr -> peekByteOff hsc_ptr 72) ptr) >>= peekArray (toInt numConstraints))
{-# LINE 93 "AigerTools.hsc" #-}
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

  alignment _ = 8
{-# LINE 109 "AigerTools.hsc" #-}

  sizeOf _ = (32)
{-# LINE 111 "AigerTools.hsc" #-}

  peek ptr = return AigerLit `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 113 "AigerTools.hsc" #-}

  poke ptr (AigerLit int) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr int
{-# LINE 116 "AigerTools.hsc" #-}

-- AigerLatch
data AigerLatch = AigerLatch { lit   :: CUInt
                             , next  :: CUInt
                             , reset :: CUInt
                             }

instance Storable AigerLatch where

  alignment _ = 8
{-# LINE 126 "AigerTools.hsc" #-}

  sizeOf _ = (32)
{-# LINE 128 "AigerTools.hsc" #-}

  peek ptr = return AigerLatch
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 131 "AigerTools.hsc" #-}
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr)
{-# LINE 132 "AigerTools.hsc" #-}
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 133 "AigerTools.hsc" #-}
  
  poke ptr latch = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr $ lit(latch)
{-# LINE 136 "AigerTools.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr $ next(latch)
{-# LINE 137 "AigerTools.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr $ reset(latch)
{-# LINE 138 "AigerTools.hsc" #-}

-- The aiger_and struct datatype
data AigerAnd = AigerAnd { lhs  :: CUInt
                         , rhs0 :: CUInt
                         , rhs1 :: CUInt
                         }

instance Storable AigerAnd where

  alignment _ = 4
{-# LINE 148 "AigerTools.hsc" #-}
  
  sizeOf _ = (12)
{-# LINE 150 "AigerTools.hsc" #-}

  peek ptr = return AigerAnd
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 153 "AigerTools.hsc" #-}
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr)
{-# LINE 154 "AigerTools.hsc" #-}
             `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 155 "AigerTools.hsc" #-}

  poke ptr and = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr $ lhs(and)
{-# LINE 158 "AigerTools.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr $ rhs0(and)
{-# LINE 159 "AigerTools.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr $ rhs1(and)
{-# LINE 160 "AigerTools.hsc" #-}

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
