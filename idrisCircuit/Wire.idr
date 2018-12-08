module Wire

import Data.Vect

public export
data CirFunc : (m:Nat) -> Type where
  M_CirFunc : String -> Nat -> (Vect m Bits32 -> Bits32) -> CirFunc m

data Wire : {m:Nat} -> (n:Nat) -> Type
data Wires : {m:Nat} -> (ts:Vect m Nat) -> Type

public export
data MetaWire : {ts:Vect m Nat} -> (m:Nat) -> Type where
  M_MetaWire : CirFunc m -> Wires ts -> MetaWire {ts=ts} m  

public export
data Wire : {m:Nat} -> (n:Nat) -> Type where
  M_Wire : MetaWire m -> Nat -> Wire n

public export
data Wires : {m:Nat} -> (ts:Vect m Nat) -> Type where
  Nil : Wires []
  (::) : Wire n -> Wires ts -> Wires (n::ts)


 
export
makeWire : {widthFunc:Vect m Nat -> Nat} -> CirFunc m -> Wires ts -> Wire (widthFunc ts)
makeWire func params  =
  M_Wire (M_MetaWire func params) 0 


{-
subWire : {width':Int} -> Wire width -> Int -> Wire (toNat (if width' >= 0 then width' else width' + (toIntNat width)))
subWire (M_Wire metaWire offset) offset' =
  M_Wire metaWire (offset+offset'') 
  where offset'' = toNat $ if offset'>= 0 then offset' else offset'+ (toIntNat width)
-}



