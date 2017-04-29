module Main where

import Elysian.Network
import qualified Data.ByteString.Char8 as BSC


-- | Our server config

serverConfig :: ServerConfig
serverConfig = defaultConfig
                 { _listenPort = PortNumber 12345
                 , _clientConnected = \c -> putStrLn $ "Client " ++ show c ++ " connected"
                 , _clientDisconnected = \c -> putStrLn $ "Client " ++ show c ++ " disconnected"
                 , _clientMessage = \c m -> putStrLn $ "Message from " ++ show c ++ ": " ++ BSC.unpack m
                 }

-- | Start our listening server

main = do
  server <- startListening serverConfig
  putStrLn "Server started"
