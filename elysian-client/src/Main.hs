-- | 
-- Copyright   :  (c) 2016 Caitlin Wilks
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

module Main where


import Control.Concurrent
import Elysian.Network.Client
import qualified Data.ByteString.Char8 as BSC


main :: IO ()
main = do
  client <- connect "127.0.0.1" 12345
  case client of
       Just c -> clientThread c
       Nothing -> putStrLn "Failed to connect to server"


clientThread :: Client -> IO ()
clientThread c =
  -- Main loop
  let loop = do
      sendMessage c $ BSC.pack "hello there"

      -- wait a second
      threadDelay 1000000
      loop
  in loop
