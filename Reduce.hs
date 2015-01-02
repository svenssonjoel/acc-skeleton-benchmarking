{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-} 
module Main where

import Data.Array.Accelerate  hiding ((!!),size)
import Data.Array.Accelerate.CUDA
import Control.Monad

import Control.Exception

import System.Environment

import Prelude hiding (fromIntegral,all)
import qualified Prelude as P

reduce :: Vector Word32 -> Acc (Scalar Word32)
reduce arr = fold (+) 0 (use arr)


main :: IO () 
main = do

  args <- getArgs

  let size' = read (args !! 0) :: Int
      size  = if size' <= 0 then 1 else size'  
      iters = read (args !! 1) :: Int
      look  = read (args !! 2) :: Int 

  let arrs = [fromList (Z:.size) (P.replicate size (P.fromIntegral (i+1)))
             | i <- [0..(iters-1)]] 

  
  (all :: [Scalar Word32]) <- forM [0..(iters-1)] $ \ix -> do
    a <- evaluate $ run (reduce (arrs !! ix))
    return a
    
  putStrLn $ "Running " P.++ show iters P.++ " iterations."
  putStrLn $ "Input data size: " P.++  show size 
  putStrLn $ "Look result: " P.++ show (all !! look) 
     
