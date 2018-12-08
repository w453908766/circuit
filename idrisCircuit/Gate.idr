module Gate

import Wire
import Data.Vect

andGate : Wires ts -> Wire (head ts)
andGate wires = makeWire (M_CirFunc "And" 2 (foldl1 prim__andB32)) wires



orGate : Wires ts -> Wire (head ts)
orGate wires = makeWire (M_CirFunc "Or" 2 (foldl1 prim__orB32)) wires



nandGate : Wires ts -> Wire (head ts)
nandGate wires = makeWire (M_CirFunc "Nand" 2 (prim__complB32 . (foldl1 prim__andB32))) wires



norGate : Wires ts -> Wire (head ts)
norGate wires = makeWire (M_CirFunc "Nor" 2 (prim__complB32 . (foldl1 prim__orB32))) wires



xorGate : Wires ts -> Wire (head ts)
xorGate wires = makeWire (M_CirFunc "Xor" 5 (foldl1 prim__xorB32)) wires


xnorGate : Wires ts -> Wire (head ts)
xnorGate wires = makeWire (M_CirFunc "Xnor" 5 (prim__complB32 . (foldl1 prim__xorB32))) wires


notGate : Wire n -> Wire n
notGate wire = makeWire (M_CirFunc "Not" 2 (prim__complB32 . head)) [wire]



