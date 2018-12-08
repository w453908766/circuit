module CRuntime (cirInit, setSignal, getSignal, click, makeMetaWireSet) where

import Wire
import FuncLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace


type MetaWireMap s = Map.Map MetaWire s

insertMetaWireSet :: Wire -> Set.Set MetaWire -> Set.Set MetaWire
insertMetaWireSet (Wire metawire _ _) metaSet =
  trace "insert" $
  if Set.member metawire metaSet then trace "have" $ metaSet
  else trace "haven't" $ makeMetaWireSet' (getParams metawire) (Set.insert metawire metaSet)

makeMetaWireSet' :: [Wire] -> Set.Set MetaWire -> Set.Set MetaWire
makeMetaWireSet' wires metaset = trace ("make" ++ (show wires) ++ (show metaset)) $ foldr insertMetaWireSet metaset wires

makeMetaWireSet wires = makeMetaWireSet' wires Set.empty

insertSpreadMap :: MetaWire -> MetaWireMap (Set.Set MetaWire) -> MetaWireMap (Set.Set MetaWire)
insertSpreadMap metawire spreadmap = 
  foldr (flip consMap metawire) spreadmap (map getMetaWire (getParams metawire)) 

makeSpreadMap :: Set.Set MetaWire -> MetaWireMap [MetaWire]
makeSpreadMap metawires = Map.map Set.toList $ foldr insertSpreadMap Map.empty metawires



type TimeBase = Map.Map (Int, MetaWire) Int
type CirState = (TimeBase, MetaWireMap Int, MetaWireMap [MetaWire])

getSignal' :: Wire -> State CirState Int
getSignal' (Wire metawire offset width) = do
  traceM "getSignal"
  (_, signalMap, _) <- get
  let msignal = Map.findWithDefault 0 metawire signalMap
  return $ getBits msignal offset width          

insertTimebase :: Int -> MetaWire -> State CirState ()
insertTimebase time metawire@(MetaWire (CirFunc _ func late) params _) = do
  traceM "insertTimebase"
  (timebase, signalMap, spreadMap) <- get
  param_values <- mapM getSignal' params
  let widths = map getWidth params
  let newSignal = func param_values
  put (Map.insert (time+late, metawire) newSignal timebase, signalMap, spreadMap)
  


    

run :: State CirState ()
run = do
  traceM "run"
  (timebase, signalMap, spreadMap) <- get
  traceM "run0"
  if Map.null timebase then return ()
  else do
    traceM "run1"
    let (((time, metawire), newValue), timebase') = Map.deleteFindMin timebase   
    let nowValue = Map.findWithDefault 0 metawire signalMap
    if nowValue == newValue 
    then do 
      traceM "run2"
      put (timebase', signalMap, spreadMap)
      run
    else do
      traceM "run3"
      put (timebase', Map.insert metawire newValue signalMap, spreadMap)
      mapM_ (insertTimebase time) (Map.findWithDefault [] metawire spreadMap)
      run  



cirRun :: [MetaWire] -> [Int] -> State CirState ()
cirRun inputs values = do
  let inputsTimebase = Map.fromList $ zip (zip (repeat 0) inputs) values
  (_, signalMap, spreadMap) <- get
  put (inputsTimebase, signalMap, spreadMap)
  run



cirInit :: [Wire] -> CirState
cirInit outputs = cirState
  where metaWireSet = makeMetaWireSet outputs
        inputs = Set.toList $ Set.filter (null . getParams) metaWireSet
        inits = map (\input-> (getFunc $ getCirFunc input) []) inputs
        spreadMap = makeSpreadMap metaWireSet
        (_, cirState) = runState (cirRun inputs inits) (Map.empty, Map.empty, spreadMap)  
  

setSignal :: CirState -> Wire -> Int -> CirState
setSignal cirState input signal = snd $ runState (cirRun [getMetaWire input] [signal]) cirState

getSignal :: CirState -> Wire -> Int
getSignal cirState wire = fst $ runState (getSignal' wire) cirState 

click :: CirState -> Wire -> CirState
click cirState button = setSignal (setSignal cirState button 1) button 0


