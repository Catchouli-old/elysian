-- | 
-- Copyright   :  (c) 2016 Caitlin Wilks
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Main where

import Control.Monad (forM)
import FRP.Elerea.Param


-- | A version of stateful that doesn't delay the input by 1

stateful' :: a -> (p -> a -> a) -> SignalGen p (Signal a)
stateful' initial f = do
  transfer initial
           (\input _ value -> f input value)
           (pure undefined)


--createHistory = newIORef []

network :: SignalGen p (Signal Bool)
network input = do



main :: IO ()
main = do
  smp <- start (stateful' "" (:))
  res <- forM "olleh~" smp
  print res
