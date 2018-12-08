{-# LANGUAGE MagicHash, BangPatterns #-}

module FuncLib
(ceildiv, flrdiv, ceildiv2, flrdiv2, getBits, consMap, first, unsafeAddr) where

import GHC.Exts
import Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set

ceildiv = div
flrdiv n m = div (n+m-1) m

ceildiv2 n = div n 2
flrdiv2 n = div (n+1) 2

getBits val low width = ((shiftL 1 width)-1 ) .&. (shiftR val low)

consMap :: (Ord a, Ord b) => a -> b -> Map.Map a (Set.Set b) -> Map.Map a (Set.Set b)
consMap key x map0 =
  case Map.lookup key map0 of
    Nothing -> Map.insert key (Set.singleton x) map0
    Just xset -> Map.insert key (Set.insert x xset) map0
    
first (a,_,_) = a



-- A datatype that has the same layout as Word and so can be casted to it.
data Ptr' a = Ptr' a

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: Any -> Word#
aToWord# a = let !mb = Ptr' a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Int
unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))

