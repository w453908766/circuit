--module CRuntime (

import Wire
import Gate
import FuncLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Array.IArray as IArray 
import qualified Control.Monad.State as State
import qualified Control.Monad.ST as ST1
import Data.IORef
import Control.Monad
--import qualified Data.Array.IO as IOArray
import Data.Array.IO



a = pin "a" 4
b = pin "b" 4
c = pin "c" 4
d = pin "d" 4
out0 = or2 a (and2 b c)
out1 = and2 a b
out2 = comb b c
 
inputs = [a,b,c]
outputs = [out0,out1,out2]

cs = [1,2,3]




timemod = 128

type MetaWireMap s = Map.Map MetaWire s

insertMetaWireSet :: Wire -> Set.Set MetaWire -> Set.Set MetaWire
insertMetaWireSet (Wire metawire _ _) metaSet =
  if Set.member metawire metaSet then metaSet
  else makeMetaWireSet (getParams metawire) (Set.insert metawire metaSet)

makeMetaWireSet :: [Wire] -> Set.Set MetaWire -> Set.Set MetaWire
makeMetaWireSet wires metaset = foldr insertMetaWireSet metaset wires

insertSpreadMap :: MetaWire -> MetaWireMap (Set.Set MetaWire) -> MetaWireMap (Set.Set MetaWire)
insertSpreadMap metawire spreadmap =
  foldr (flip consMap metawire) spreadmap (map getMetaWire (getParams metawire)) 

makeSpreadMap :: Set.Set MetaWire -> MetaWireMap [MetaWire]
makeSpreadMap metawires = Map.map Set.toList $ foldr insertSpreadMap Map.empty metawires

metaWireSet = makeMetaWireSet outputs Set.empty
spreadMap = makeSpreadMap metaWireSet

makeSignalRefMap :: Set.Set MetaWire -> IO (MetaWireMap (IORef Int))
makeSignalRefMap metawires = do
  refs <- sequence $ replicate (Set.size metawires) (newIORef 0)
  return $ Map.fromList $ zip (Set.toList metawires) refs 


getSignal :: MetaWireMap (IORef Int) -> Wire -> IO Int
getSignal signalRefMap (Wire metawire offset width)  = do
  msignal <- readIORef $ signalRefMap Map.! metawire
  return $ getBits msignal offset width          


insertTimebase :: IOArray Int (MetaWireMap Int) -> Int -> MetaWireMap (IORef Int) -> MetaWire -> IO Int
insertTimebase timebase time signalRefMap metawire@(MetaWire _ (CirFunc func late) params) = do
  let acttime = mod (time+late) timemod
  let widths = map getWidth params
  param_values <- sequence $ map (getSignal signalRefMap) params
  mws <- readArray timebase acttime
  let exist = Map.member metawire mws
  writeArray timebase acttime (Map.insert metawire (func param_values widths) mws)
  return $ if exist then 0 else 1
  



updateValues :: IOArray Int (MetaWireMap Int) -> Int -> MetaWireMap (IORef Int) -> (MetaWire, Int) ->  IO Int
updateValues timebase time signalRefMap (metawire, newValue) = do
--  print "updateValues"
  let valueRef = signalRefMap Map.! metawire 
  nowValue <- readIORef valueRef
  if newValue==nowValue then return (-1)
  else do 
    writeIORef valueRef newValue
    exists <- sequence $ map (insertTimebase timebase time signalRefMap) (Map.findWithDefault [] metawire spreadMap)
    return $ (sum exists)-1 

run :: IOArray Int (MetaWireMap Int) -> Int -> Int -> MetaWireMap (IORef Int) -> IO ()
run _ 0 _ _ = return ()
run timebase n time signalRefMap = do
  --print "run"
  id_values <- readArray timebase time
  if id_values==Map.empty then run timebase n (mod (time+1) timemod) signalRefMap
  else do
    writeArray timebase time Map.empty
    --m <- updateValues timebase n time (Map.toList id_values)
   
    n'list <- sequence $ map (updateValues timebase time signalRefMap) (Map.toList id_values)
    
    run timebase (n+(sum n'list)) time signalRefMap
    



main :: IO ()
main = do

  
  timebase <- newArray (0,timemod-1) Map.empty
  signalRefMap <- makeSignalRefMap metaWireSet
  let inputs' = map getMetaWire inputs
  
  --forever $ 
  do
    putStr "> "
  --  cmdLine<-getLine
  --  let cs = (read cmdLine) :: [Int]
    
    writeArray timebase 0 $ Map.fromList $ zip inputs' cs
    run timebase (length inputs) 0 signalRefMap
    output_values <- sequence $ map (getSignal signalRefMap) outputs
    print output_values
  
