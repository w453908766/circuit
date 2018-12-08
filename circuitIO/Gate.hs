module Gate
(andGate, orGate, nandGate, norGate, xorGate, notGate, and2, or2, pin, combines, comb) where

import Wire
import FuncLib
import Data.Bits
import Debug.Trace


andGate wires = 
  makewire "And" (CirFunc (firstParam . (foldl1 (.&.))) 2) wires head

orGate wires =
  makewire "Or" (CirFunc (firstParam . (foldl1 (.|.))) 2) wires head

nandGate wires = 
  makewire "Nand" (CirFunc (firstParam . complement . (foldl1 (.&.))) 2) wires head

norGate wires = 
  makewire "Nor" (CirFunc (firstParam . complement . (foldl1 (.|.))) 2) wires head
  
xorGate wires = 
  makewire "Xor" (CirFunc (firstParam . (foldl1 xor)) 5) wires head

notGate wire = 
  makewire "Not" (CirFunc (firstParam . complement . head) 1) [wire] head

and2 w0 w1 = andGate [w0, w1]
or2 w0 w1 = orGate [w0, w1]

pin name width = makewire name (CirFunc (\_ _-> 0) 0) [] (\_ -> width)

combines wires = makewire "Combine" (CirFunc combFunc 0) wires sum

comb high low = combines [high, low]

combFunc :: [Int] -> [Int] -> Int
combFunc signals (_:widths) = --trace ((show signals) ++ (show widths)) $ 
  sum $ zipWith shiftL signals $ scanr (+) 0 widths
