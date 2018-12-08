import Wire
import Gate
import CRuntime
import ChipLib
import FuncLib

import qualified Data.Map as Map





a = pin "a" 8 0
b = pin "b" 8 0
c = pin "c" 3 0 

shr = shiftLer a c

inputs = [a,c]
outputs = [shr]
-------------------------------

cirState = cirInit outputs 
  
