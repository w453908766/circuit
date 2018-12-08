module CRuntime (cirInit, cirRun) where

import Wire
import FuncLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace



type TimeBase = Map.Map (Int, MetaWire) Int
type MetaWireMap s = Map.Map MetaWire s
type CirState = (TimeBase, MetaWireMap Int, MetaWireMap [MetaWire])


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



getSignal :: Wire -> State CirState Int
getSignal (Wire metawire offset width) = do
  (_, signalMap, _) <- get
  let msignal = Map.findWithDefault 0 metawire signalMap
  return $ getBits msignal offset width          


insertTimebase :: Int -> MetaWire -> State CirState ()
insertTimebase time metawire@(MetaWire _ (CirFunc func late) params) = do
  (timebase, signalMap, spreadMap) <- get
  param_values <- sequence $ map getSignal params
  let widths = map getWidth params
  let newSignal = func param_values widths
  put (Map.insert (time+late, metawire) newSignal timebase, signalMap, spreadMap)
  


    

run :: State CirState ()
run = do
  (timebase, signalMap, spreadMap) <- get
  if Map.null timebase then return ()
  else do
    let (((time, metawire), newValue), timebase') = Map.deleteFindMin timebase   
    let nowValue = Map.findWithDefault 0 metawire signalMap
    if nowValue == newValue 
    then do 
      put (timebase', signalMap, spreadMap)
      run
    else do
      put (timebase', Map.insert metawire newValue signalMap, spreadMap)
      sequence $ map (insertTimebase time) (Map.findWithDefault [] metawire spreadMap)
      run  




cirInit :: [Wire] -> [Wire] -> CirState
cirInit inputs outputs = cirState
  where metaWireSet = makeMetaWireSet outputs Set.empty
        spreadMap = makeSpreadMap metaWireSet
        (_, cirState) = cirRun inputs (repeat 0) outputs (Map.empty, Map.empty, spreadMap)




cirRun' :: [Wire] -> [Int] -> [Wire] -> State CirState (Map.Map Wire Int)
cirRun' inputs values outputs = do
  let inputs' = map getMetaWire inputs
  let inputsTimebase = Map.fromList $ zip (map (\x->(0,x)) inputs') values
  (_, signalMap, spreadMap) <- get
  put (inputsTimebase, signalMap, spreadMap)
  run
  outputResults <- sequence $ map getSignal outputs
  return $ Map.fromList $ zip outputs outputResults
  
cirRun :: [Wire] -> [Int] -> [Wire] -> CirState -> ((Map.Map Wire Int), CirState)
cirRun inputs values outputs cirState = runState (cirRun' inputs values outputs) cirState
    


 



