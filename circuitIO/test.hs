import Control.Monad.ST
import Data.STRef
import Data.IORef
import Data.Array.IO
import System.Random
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set 
import Data.List as List


{-          STRef
fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0
  b <- newSTRef 1
  repeatFor n
    (do x <- readSTRef a
        y <- readSTRef b
        writeSTRef a y
        writeSTRef b (x+y))
  readSTRef a
  
fib :: Int -> Integer
fib n = runST (fibST n)


repeatFor n = foldr (>>) (return ()) . replicate n
-}




{-         IORef
main :: IO ()
main = do
  ii <- getLine
  let i = (read ii) ::Int
  
  
  a <- newIORef 10

  b <- readIORef a
  writeIORef a (i+b)
  c <- readIORef a
  print c
  
-}
{-           STArray
qqq = runST $ do
  arr <- (newListArray (0, 8) [0..]) :: ST s (STArray s Int Int)
  writeArray arr 0 10
  getElems arr
-}

{-         IOArraay
main :: IO ()
main = do
  arr <- newListArray (0,10) [20..] :: IO (IOArray Int Int)
  aaaa <- getElems arr
  print aaaa
-}

{-
main :: IO ()
main = do
  let arr = ((newListArray (0, 8) [0..]) :: ST s (STArray s Int Int))
  let aaa = runST (arr >>= getElems)
  print aaa
  
-}
{-
main :: IO ()
main = do
  qqq <- return $ runST $ do
    arr <- (newListArray (0, 8) [0..]) :: ST s (STArray s Int Int)
    writeArray arr 0 10
    getElems arr
  print qqq
  
-}
{-
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  foldr (>>) (return ()) [push 2,push 10,push 12]
  pop
  pop
  [pop,pop]
  
aaa = runState stackManip [5,8,2,1]
-}


type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = do
  xs <- get
  put (a:xs)

stackManip :: State Stack Int
stackManip = do
--  foldr (>>) (return ()) [push 2,push 10,push 12]
  pop
  ws <- sequence [pop,pop]
  return $ (ws!!0) + (ws!!1)
  
aaa = runState stackManip [5,8,2,1]

