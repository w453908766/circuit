module Gate
(andGate, orGate, nandGate, norGate, xorGate, xnorGate, notGate, 
and2, or2, nand2, nor2, xor2, xnor2, 
pin, combines, comb, chip) where

import Wire
import FuncLib
import Data.Bits
import Data.Hashable
import Debug.Trace


andGate wires = 
  makewire (CirFunc "And" (firstParam . (foldl1 (.&.))) 2) wires head

orGate wires =
  makewire (CirFunc "Or" (firstParam . (foldl1 (.|.))) 2) wires head

nandGate wires = 
  makewire (CirFunc "Nand" (firstParam . complement . (foldl1 (.&.))) 2) wires head

norGate wires = 
  makewire (CirFunc "Nor" (firstParam . complement . (foldl1 (.|.))) 2) wires head
  
xorGate wires = 
  makewire (CirFunc "Xor" (firstParam . (foldl1 xor)) 5) wires head
  
xnorGate wires = 
  makewire (CirFunc "Xnor" (firstParam . complement . (foldl1 xor)) 5) wires head  

notGate wire = 
  makewire (CirFunc "Not" (firstParam . complement . head) 1) [wire] head

and2 w0 w1 = andGate [w0, w1]
or2 w0 w1 = orGate [w0, w1]
nand2 w0 w1 = nandGate [w0, w1]
nor2 w0 w1 = norGate [w0, w1]
xor2 w0 w1 = xorGate [w0, w1]
xnor2 w0 w1 = xnorGate [w1, w1]


pin name width init = makewire (CirFunc name (\_ _-> init) 0) [] (\_ -> width)

combines wires = makewire (CirFunc "Combine" combFunc 0) wires sum

comb high low = combines [high, low]

combFunc :: [Int] -> [Int] -> Int
combFunc signals (_:widths) = sum $ zipWith shiftL signals $ scanr (+) 0 widths

chip :: String -> [Wire] -> Wire -> Wire
chip chipName chipParams wire =
  Wire (MetaWire id (CirFunc chipName (firstParam . head) 0) [wire] chipParams) 0 (getWidth wire)
  where id = hash (chipName, hashWire wire)
  
  