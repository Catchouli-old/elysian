module Main where

import Elysian.Network
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC


-- | Our server config

serverConfig :: ServerConfig
serverConfig = defaultConfig
                 { _listenPort = PortNumber 12345
                 , _clientConnected = clientConnected
                 , _clientDisconnected = clientDisconnected
                 , _clientMessage = clientMessage
                 }


-- | Client connected handler

clientConnected :: ClientId -> IO ()
clientConnected c = putStrLn $ "Client " ++ show c ++ " connected"


-- | Client connected handler

clientDisconnected :: ClientId -> IO ()
clientDisconnected c = putStrLn $ "Client " ++ show c ++ " disconnected"


-- | Client connected handler

clientMessage :: ClientId -> BS.ByteString -> IO ()
clientMessage c m = putStrLn $ "Message from " ++ show c ++ ": " ++ BSC.unpack m


-- | Start our listening server

main = do
  -- Start listen server
  server <- startListening serverConfig

  -- Main loop
  let loop = do
      a <- getLine
      case a of
           "q" -> stopListening server
           _   -> return ()
      quitting <- isQuitting server
      unless quitting loop
  loop
