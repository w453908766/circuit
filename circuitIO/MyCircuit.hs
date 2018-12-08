import Wire
import Gate
import CRuntime
import ClipLib

import qualified Data.Map as Map

fullAdder :: Wire -> Wire -> Wire -> Wire
fullAdder a b c =
  comb (orGate [and2 a b, and2 a c, and2 b c]) (xorGate [a, b, c])
  
adder a b c =
  if (getWidth a)==0 then c
  else adder (rmlow a) (rmlow b) (comb (fullAdder (low a) (low b) (high c)) (rmhigh c))


cbuffer e wire = and2 wire $ combines $ replicate (getWidth wire) e

mif c w1 w0 = or2 (cbuffer (notGate c) w0) (cbuffer c w1)


--------------------------
a = pin "a" 2
b = pin "b" 2
c = pin "c" 1

out = adder a b c


inputs = [a,b]
outputs = [out]
-------------------------------



loop env = do
  putStrLn "input:"
  cs <- getLine
  let cmdLine = (read cs) :: [Int]
  let (values, env') = cirRun inputs cmdLine outputs env
  print $ Map.elems values
  loop env'
   
main :: IO ()
main = do
  loop $ cirInit inputs outputs 
  
