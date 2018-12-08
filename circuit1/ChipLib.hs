module ChipLib (
bits0, bits1, bit0, bit1,
cbuffer, mif, extend0L, extend0H,
adder, suber, more, equGate, multier, adder',
shiftRer, shiftLer
) where

import FuncLib
import Gate
import Wire
import Data.Bits


bits0 = pin "bit0" 32 0
bits1 = pin "bit1" 32 (-1)

bit0 = low bits0
bit1 = low bits1

cbuffer e wire = and2 wire $ combines $ replicate (getWidth wire) e
mif c w1 w0 = chip "MIF" [c,w1,w0] $ or2 (cbuffer (notGate c) w0) (cbuffer c w1)

adder' a b c
  |(getWidth a)==1  = (xorGate [a,b,c], and2 a b, or2 a b)
  |otherwise = (comb f1 f0, or2 g1 (and2 p1 g0), and2 p1 p0)
   where (f0, g0, p0) = adder' (lowhf a) (lowhf b) c
         (f1, g1, p1) = adder' (highhf a) (highhf b) (or2 g0 $ and2 p0 c)

adder a b = chip "Adder" [a,b] $ first $ adder' a b bit0
adderC a b =
  comb g f
  where (f, g, p) = adder' a b bit0
suber a b = chip "Suber" [a,b] $ first $ adder' a (notGate b) bit1
more a b = chip "More" [a,b] $ high $ suber b a
equGate a b = chip "Equ" [a,b] $ andGate $ bitList (xnor2 a b)

extend0L wire n = comb wire (subwire bits0 0 (n-(getWidth wire)))
extend0H wire n = comb (subwire bits0 0 (n-(getWidth wire))) wire



multier a b =
  chip "Multier" [a,b] $
  if (getWidth b)==1 then cbuffer b a
  else adder hf' lf'
   where hf = multier a (highhf b)
         lf = multier a (lowhf b)
         hf' = extend0H (comb hf (lows bits0 (flrdiv2 $ getWidth b))) width'  
         lf' = extend0H lf width'
         width' = (getWidth a) + (getWidth b)

shiftRer wire k =
  chip "shiftR" [wire, k] $
  if 0==(getWidth k) then wire
  else shiftRer wire' (rmhigh k)
   where wid = shiftL 1 ((getWidth k)-1)
         bw = (getWidth wire) - wid
         wire' = mif (high k) (extend0H (subwire wire wid bw) (getWidth wire)) wire

shiftLer wire k =
  chip "shiftL" [wire, k] $
  if 0==(getWidth k) then wire
  else shiftLer wire' (rmhigh k)
   where wid = shiftL 1 ((getWidth k)-1)
         bw = (getWidth wire) - wid
         wire' = mif (high k) (extend0L (subwire wire 0 bw) (getWidth wire)) wire
