{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-} 
module Main where

import Data.Array.Accelerate  hiding ((!!))
import Data.Array.Accelerate.CUDA
import Control.Monad

import Control.Exception

import Prelude hiding (fromIntegral)
import qualified Prelude as P

reduce :: Vector Word32 -> Acc (Scalar Word32)
reduce arr = fold (+) 0 (use arr)

millions = 16000000
afew = 100

arrs = [fromList (Z:.millions) (P.replicate millions i)
       | i <- [0..999]] 


main :: IO () 
main = do
  (all :: [Scalar Word32]) <- forM [0..9] $ \ix -> do
    let a = run (reduce (arrs !! ix))
    return a

  putStrLn $ show all
     
