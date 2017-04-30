{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Elysian.Network.Client
  ( Client
  , connect
  , sendMessage
  )
where

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Concurrent
import Control.Monad (void)
import Control.Exception.Safe (bracketOnError, catchAny)
import qualified Data.ByteString as BS
import qualified Network as NS (PortID)
import qualified Network.BSD as NS (getProtocolNumber, getHostByName, hostAddress)
import qualified Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NS


-- | Connection info

data ClientState = ClientState
                  { _socket :: NS.Socket
                  , _sockAddr :: NS.SockAddr
                  }
makeLenses ''ClientState


-- | An encapsulation for the client state that we can pass out of this module

data Client = Client (MVar ClientState)


-- | Client config

data ClientConfig = ClientConfig
                      { _hostname :: NS.HostName
                      , _port :: NS.PortID
                      , _connectionStarted :: Client -> IO ()
                      , _connectionLost :: Client -> IO ()
                      , _serverMessage :: Client -> BS.ByteString -> IO ()
                      }


-- | The default config that does nothing

defaultConfig :: ClientConfig
defaultConfig = ClientConfig
                  { _hostname = ""
                  , _port = NS.PortNumber 0
                  , _connectionStarted = \_ > return ()
                  , _connectionLost = \_ -> return ()
                  , _serverMessage = \_ _ -> return ()
                  }


-- | Connect to an address, returning a socket

connectTo :: NS.SockAddr -> IO NS.Socket
connectTo addr = do
  proto <- NS.getProtocolNumber "tcp"
  bracketOnError
    (NS.socket NS.AF_INET NS.Stream proto)
    (NS.close)
    (\sock -> do
      NS.connect sock addr
      return sock
    )


-- | Connect to a server and port, returning a Client

connect :: NS.HostName -> NS.PortNumber -> IO (Maybe Client)
connect hostname port =
  let makeConnection = do
      -- lookup hostname
      he <- NS.getHostByName hostname
      let sockAddr = NS.SockAddrInet port (NS.hostAddress he)

      -- Create socket
      socket <- connectTo sockAddr

      -- Return client state
      clientState <- newMVar $ ClientState socket sockAddr
      return $ Just (Client clientState)
  in catchAny makeConnection (const . return $ Nothing)


-- | Send a message to the server

sendMessage :: Client -> BS.ByteString -> IO ()
sendMessage (Client state) msg = do
  clientState <- readMVar state
  void $ NS.send (clientState ^. socket) msg
