module Wire
  (CirFunc(..), MetaWire(..), Wire(..), subwire, makewire, 
   low, high, rmlow, rmhigh, lwhalf, hihalf, 
   lwhalf_cl, hihalf_cl, wire_ref) where

import FuncLib
import Data.Hashable


data CirFunc = CirFunc {signalfunc :: [Int] -> [Int] ->Int, latency :: Int}

data MetaWire = MetaWire {  getID :: Int,
                            getFunc :: CirFunc,
                            getParams :: [Wire]
                         }
instance Eq MetaWire where
  a == b = (getID a) == (getID b)
  
instance Ord MetaWire where
  compare a b = compare (getID a) (getID b)

instance Show MetaWire where
  show = show . getID  
  
data Wire = Wire { --getName :: String,
                   getMetaWire :: MetaWire,
                   getOffset :: Int,
                   getWidth :: Int
                 } deriving (Eq, Ord)
                 
--instance Show Wire where
--  show = show . getMetaWire

subwire (Wire metawire offset _) offset' width'
 = Wire metawire (offset+offset') width'

makewire :: String -> CirFunc -> [Wire] -> ([Int] -> Int) -> Wire
makewire name func params widthFunc = 
  Wire (MetaWire id func params) 0 width
  where id = hash $ (hash name) + (hash (map (getID . getMetaWire) params))
        width =  widthFunc $ map getWidth params 
        
low wire = subwire wire 0 1
high wire = subwire wire ((getWidth wire)-1) 1
rmlow wire = subwire wire 1 ((getWidth wire)-1) 
rmhigh wire = subwire wire 0 ((getWidth wire)-1) 
lwhalf wire@(Wire _ _ width) = subwire wire 0 (flrdiv2 width)
hihalf wire@(Wire _ _ width) = subwire wire (ceildiv2 width) (flrdiv2 width)
lwhalf_cl wire@(Wire _ _ width) = subwire wire 0 (ceildiv2 width)
hihalf_cl wire@(Wire _ _ width) = subwire wire (flrdiv2 width) (ceildiv2 width)

wire_ref wire i = subwire wire i 1




