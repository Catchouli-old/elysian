module Main where

import Elysian.Network
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map.Strict as M
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

clientConnected :: Server -> ClientId -> IO ()
clientConnected s c = do
  putStrLn $ "Client " ++ show c ++ " connected"
  sendMessage s c $ BSC.pack "Welcome to the server\r\n"


-- | Client connected handler

clientDisconnected :: Server -> ClientId -> IO ()
clientDisconnected s c = do
  putStrLn $ "Client " ++ show c ++ " disconnected"


-- | Client connected handler

clientMessage :: Server -> ClientId -> BS.ByteString -> IO ()
clientMessage s c m = do
  putStrLn $ "Message from " ++ show c ++ ": " ++ BSC.unpack m
  sendMessage s c $ BSC.pack "Thank you for your message\r\n"


-- | Start our listening server

main = do
  -- Start listen server
  server <- startListening serverConfig

  -- Console loop
  let console = do
      a <- getLine
      case a of
           "q" -> stopListening server
           "c" -> getClients server >>= \clients -> mapM_ (putStrLn . show) (M.keys clients)
           _   -> return ()

      quitting <- isQuitting server
      unless quitting console
  forkIO console

  -- Main loop
  let loop = do
      -- Update clients
      clients <- getClients server
      forM_ (M.keys clients) (\c -> sendMessage server c $ BSC.pack "Update\r\n")

      -- wait a second
      threadDelay 1000000

      -- Loop
      quitting <- isQuitting server
      unless quitting loop
  loop
