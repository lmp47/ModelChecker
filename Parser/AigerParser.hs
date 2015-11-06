{-|
Module : Parser.AigerParser
Description: A parser for AIGER formats

A parser for AIGER format
AIGER format has header:

@
  aag M I L O A (B C J F)
@

M, I, L, O, A, B, C, and F are nonnegative integers giving:

- @M@: the maximal variable index
- @I@: the number of inputs
- @L@: the number of latches
- @O@: the number of outputs
- @A@: the number of AND gates
- @B@: the number of bad states
- @C@: the number of invariant constraints
- @J@: the number of justice properties
- @F@: the number of fairness constraints

And the order that these occur in the body of an AIGER file is:

@
M I L O (B) (C) (J) (F) A
@

This parser disregards justice and fairness constraints
and also disregards any symbols and comments.
-}

module Parser.AigerParser (getModelFromFile) where

import Parser.AigModel
import Data.List.Split
import Data.ByteString.Char8 (pack, unpack, ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits

-- | Accepts a filepath for an AIGER (@aig@ or @aag@) file and
-- returns the corresponding 'AigModel.Model'.
getModelFromFile :: String -> IO Model
getModelFromFile s = do
                     aiger <- BS.readFile s
                     return $ parseAiger aiger

-----------------------
-- General Functions --
-----------------------

-- | Pad a list xs with element x until it has length n
pad xs n x = xs ++ (replicate (n - (length xs)) x)

------------
-- Parser --
------------

-- | Remove Justice and Fairness properties
removeJF :: [String] -> [Int] -> [String]
removeJF str counts =
  case counts of
  (b:c:j:f:_) -> drop f $ removeJustice j
  (b:c:j:_) -> removeJustice j
  _ -> str
  where
  removeJustice count =
    let (jCount, rest) = splitAt count str in
      drop (sum (map read jCount)) rest

-- | General parser
parseAiger :: ByteString -> Model
parseAiger file =
  let (header:rest) = (lines $ unpack file) in
    case (words header) of
    ("aag" : counts) -> parseAag rest (map read counts)
    ("aig" : counts) -> parseAig rest (map read counts)
    _ -> error "Cannot parse: Problem with header"

-- | A parser for ASCII AIGER format (.aag)
parseAag :: [String] -> [Int] -> Model
parseAag body (m:i:l:o:a:other) =
  let loabcjfs            = drop i body
      (latches, oabcjfs)  = splitAt l loabcjfs
      (outs, abcjfs)      = splitAt o oabcjfs
      (bad, acjfs)        = case other of
                            (b:_) -> splitAt b abcjfs
                            _     -> ([], abcjfs)
      (constraints, ajfs) = case other of
                            (_:c:_) -> splitAt c acjfs
                            _ -> ([], acjfs)
      (ands, _)           = splitAt a $ removeJF ajfs other
  in 
    Model { numVars     = fromIntegral m
          , numInputs   = fromIntegral i
          , latches     = [map litFromAiger $ pad (map read latch) 3 0 | latch <- (map words latches)]
          , outputs     = map (litFromAiger.read) outs
          , ands        = [map (litFromAiger.read) and | and <- (map words ands)]
          , bad         = map (litFromAiger.read) bad
          , constraints = map (litFromAiger.read) constraints
          }

parseAag _ _ = error "Cannot parse: Problem with header"

-- | A parser for binary AIGER format (.aig)
parseAig :: [String] -> [Int] -> Model
parseAig body (m:i:l:o:a:other) =
  let (nexts, oabcjfs)    = splitAt l body
      (outs, abcjfs)      = splitAt o oabcjfs
      (bad, acjfs)        = case other of
                            (b:_) -> splitAt b abcjfs
                            _     -> ([], abcjfs)
      (constraints, ajfs) = case other of
                            (_:c:_) -> splitAt c acjfs
                            _ -> ([], acjfs)
      (ands, _)           = splitAt a $ removeJF ajfs other
  in
  Model { numVars     = fromIntegral m
        , numInputs   = fromIntegral i
        , latches     = zipWith (:)
                        (makeLiterals (fromIntegral $ i) (fromIntegral l))
                        [map litFromAiger $ pad (map read latch) 2 0 | latch <- (map words nexts)]
        , outputs     = map (litFromAiger.read) outs
        , ands        = parseAnds a (fromIntegral $ 2*(i + l)) (pack $ unlines ands) []
        , bad         = map (litFromAiger.read) bad
        , constraints = map (litFromAiger.read) constraints
        }
  where
  makeLiterals :: Word -> Word -> [Lit]
  makeLiterals s n = map Var [s..s+n-1]
  parseAnds num lit bytes acc =
    case num of
    0 -> [map litFromAiger [lhs, lhs - delta0, lhs - delta0 - delta1] | (lhs, (delta0, delta1)) <- acc]
    _ -> let lit'          = lit + 2
             (delta0, bs)  = parseDelta bytes []
             (delta1, bs') = parseDelta bs [] in
         parseAnds (num - 1) lit' bs' ((lit', (delta0, delta1)):acc)
  parseDelta :: ByteString -> [Word] -> (Word, ByteString)
  parseDelta bytes acc =
    let (b,bs) = (BS.head bytes, BS.tail bytes)
        acc'   = (fromIntegral (clearBit b 7)) : acc in
    if (testBit b 7) then (parseDelta bs acc') else 
    (fromIntegral $ foldl1 (+) $ zipWith (*) (map (2^) [0,7..]) (reverse acc'), bs)

parseAig _ _ = error "Cannot parse: Problem with header"
