module Wire
  (CirFunc(..), MetaWire(..), Wire(..), subwire, makewire, hashWire,
   low, high, rmlow, rmhigh, lowhf, highhf, lows, 
   lowhf', highhf', wire_ref, bitList,
   MetaWireMap, makeMetaWireSet, makeSpreadMap) where

import FuncLib
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf
import Debug.Trace

------------------------------------------
data CirFunc = CirFunc {getFuncName :: String, getFunc :: [Int] -> [Int] ->Int, getLatency :: Int}

data MetaWire = MetaWire {  getHash :: Int,
                            getCirFunc :: CirFunc,
                            getParams :: [Wire],
                            getChipParams :: [Wire] 
                         }
instance Eq MetaWire where
  a == b = (getHash a) == (getHash b)
  
instance Ord MetaWire where
  compare a b = compare (getHash a) (getHash b)

  
instance Show MetaWire where
  show (MetaWire _ (CirFunc funcName _ _) _ []) = funcName
  show (MetaWire _ (CirFunc funcName _ _) _ chipParams) =  
    printf "(%s)" $ unwords $ funcName:(map show chipParams)
  
  
  
data Wire = Wire { getMetaWire :: MetaWire,
                   getOffset :: Int,
                   getWidth :: Int
                 } deriving (Eq, Ord)
                 
instance Show Wire where
  show (Wire metawire offset width) = printf "%s[%s,%s]" (show metawire) (show offset) (show width)
  
hashWire :: Wire -> Int
hashWire (Wire metawire offset width) = hash ((getHash metawire), offset, width)
--------------------------------------
makewire :: CirFunc -> [Wire] -> ([Int] -> Int) -> Wire
makewire func params widthFunc = 
  Wire (MetaWire id func params params) 0 width
  where id = hash (getFuncName func, map hashWire params)
        width =  widthFunc $ map getWidth params 

subwire :: Wire -> Int -> Int -> Wire
subwire (Wire metawire offset width) offset' width' =
  Wire metawire (offset+offset'') width'' 
   where offset'' = if offset'>=0 then offset' else offset'+width
         width'' = if width'>=0 then width' else width'+width


low wire = subwire wire 0 1
high wire = subwire wire (-1) 1
rmlow wire = subwire wire 1 (-1) 
rmhigh wire = subwire wire 0 (-1) 
lowhf wire@(Wire _ _ width) = subwire wire 0 (flrdiv2 width)
highhf wire@(Wire _ _ width) = subwire wire (flrdiv2 width) (ceildiv2 width)
lowhf' wire@(Wire _ _ width) = subwire wire 0 (ceildiv2 width)
highhf' wire@(Wire _ _ width) = subwire wire (ceildiv2 width) (flrdiv2 width)

lows wire n = subwire wire 0 n

wire_ref wire i = subwire wire i 1

bitList wire =
  if (getWidth wire)==0 then []
  else (high wire):(bitList $ rmhigh wire)
-------------------------------------------



type MetaWireMap s = Map.Map MetaWire s

insertMetaWireSet :: Wire -> Set.Set MetaWire -> Set.Set MetaWire
insertMetaWireSet (Wire metawire _ _) metaSet =
  if Set.member metawire metaSet then metaSet
  else makeMetaWireSet' (getParams metawire) (Set.insert metawire metaSet)

makeMetaWireSet' :: [Wire] -> Set.Set MetaWire -> Set.Set MetaWire
makeMetaWireSet' wires metaset = foldr insertMetaWireSet metaset wires

makeMetaWireSet wires = makeMetaWireSet' wires Set.empty

insertSpreadMap :: MetaWire -> MetaWireMap (Set.Set MetaWire) -> MetaWireMap (Set.Set MetaWire)
insertSpreadMap metawire spreadmap = 
  foldr (flip consMap metawire) spreadmap (map getMetaWire (getParams metawire)) 

makeSpreadMap :: Set.Set MetaWire -> MetaWireMap [MetaWire]
makeSpreadMap metawires = Map.map Set.toList $ foldr insertSpreadMap Map.empty metawires
