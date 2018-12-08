import Wire
import Gate
import CRuntime
import ChipLib
import FuncLib
import qualified Data.Map as Map


a = pin "a" 8 0
b = pin "b" 8 0

z = adder a b

cs = cirInit [z]

cs' = setSignal cs a 45
cs'' = setSignal cs' b 54

r = getSignal cs'' z 


{-
a = pin "a" 8 0
e = pin "e" 1 0

z = trigger e a


cs = cirInit [z]

cs' = setSignal cs a 45


cs'' = setSignal cs' b 54

r = getSignal cs'' z 

-}

d = makewire (CirFunc "loop" (\_-> 5) 0) [d] (\_ -> 4)
set0 = makeMetaWireSet [d]

qqq = xs where xs = 1:xs
