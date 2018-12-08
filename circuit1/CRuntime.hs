module CRuntime (cirInit, setSignal, getSignal) where

import Wire
import FuncLib
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace



type TimeBase = Map.Map (Int, MetaWire) Int

type CirState = (TimeBase, MetaWireMap Int, MetaWireMap [MetaWire])




getSignal' :: Wire -> State CirState Int
getSignal' (Wire metawire offset width) = do
  (_, signalMap, _) <- get
  let msignal = Map.findWithDefault 0 metawire signalMap
  return $ getBits msignal offset width          

insertTimebase :: Int -> MetaWire -> State CirState ()
insertTimebase time metawire@(MetaWire _ (CirFunc _ func late) params _) = do
  (timebase, signalMap, spreadMap) <- get
  param_values <- mapM getSignal' params
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
        inits = map (\input-> (getFunc $ getCirFunc input) [] []) inputs
        spreadMap = makeSpreadMap metaWireSet
        (_, cirState) = runState (cirRun inputs inits) (Map.empty, Map.empty, spreadMap)  
  

setSignal :: CirState -> Wire -> Int -> CirState
setSignal cirState input signal = snd $ runState (cirRun [getMetaWire input] [signal]) cirState

getSignal :: CirState -> Wire -> Int
getSignal cirState wire = fst $ runState (getSignal' wire) cirState 




