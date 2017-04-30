{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Elysian.Network
  ( startListening
  , stopListening
  , isQuitting
  , ServerConfig(..)
  , defaultConfig
  , N.PortID(..)
  , clientHash
  , ClientId
  )
where


import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.State.Strict
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Crypto.Hash
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network as N
import qualified Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NS


-- | Information about a socket connection

data ConnInfo = ConnInfo
                  { _socket :: NS.Socket
                  , _sockAddr :: NS.SockAddr
                  }
makeLenses ''ConnInfo


-- | Client id

data ClientId = ClientId (Digest SHA1) deriving Show


-- | Information about a connected client

data Client = Client
                { _connInfo :: ConnInfo
                , _clientId :: ClientId
                }
makeLenses ''Client


-- | The server state (connected clients etc)

data ServerState = ServerState
                     { _quitSignal :: MVar Bool
                     , _clientList :: V.Vector Client
                     , _listenSock :: NS.Socket
                     }
makeLenses ''ServerState


-- | The server config (port, callbacks etc)

data ServerConfig = ServerConfig
                      { _listenPort :: N.PortID
                      , _clientConnected :: ClientId -> IO ()
                      , _clientDisconnected :: ClientId -> IO ()
                      , _clientMessage :: ClientId -> BS.ByteString -> IO ()
                      }
makeLenses ''ServerConfig


-- | A default config that does nothing

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
                  { _listenPort = N.PortNumber 0
                  , _clientConnected = \_ -> return ()
                  , _clientDisconnected = \_ -> return ()
                  , _clientMessage = \_ _ -> return ()
                  }


-- | ByteString to client hash

clientHash :: BS.ByteString -> Digest SHA1
clientHash = hash


-- | Start the listening thread, the main thread, and the command listener

startListening :: ServerConfig -> IO (MVar ServerState)
startListening serverConfig = NS.withSocketsDo $ do
  -- Signal that can be set to signal the other threads to exit
  quitSignal <- newMVar False

  -- Set up listening socket
  let port = serverConfig ^. listenPort
  listenSock <- N.listenOn $ port
  forkIO $ listenForNew serverConfig listenSock quitSignal

  -- Server state
  let defaultServerState = ServerState quitSignal V.empty listenSock
  serverState <- newMVar defaultServerState

  return serverState


-- | Stop listening

stopListening :: MVar ServerState -> IO ()
stopListening state = do
  serverState <- readMVar state
  modifyMVar_ (serverState ^. quitSignal) (return . const True)
  NS.close (serverState ^. listenSock)


-- | Whether stopListening has been called

isQuitting :: MVar ServerState -> IO Bool
isQuitting state = do
  serverState <- readMVar state
  readMVar (serverState ^. quitSignal)


-- | Listen for new clients on a listening socket, adding them to a new clients list

listenForNew :: ServerConfig -> NS.Socket -> MVar Bool -> IO ()
listenForNew serverConfig socket quitSignal = do
  -- Wait for a new connection
  (sock, sockAddr) <- NS.accept socket
  let connInfo = ConnInfo sock sockAddr
  let clientId = ClientId $ clientHash (BSC.pack . show $ sockAddr)
  let client = Client connInfo clientId

  -- Fork off a thread to process new messages
  forkIO $ processSocket serverConfig client quitSignal

  -- Client connected callback
  serverConfig ^. clientConnected $ clientId

  -- Check if quitting
  quitting <- readMVar quitSignal

  -- Loop
  unless quitting $ listenForNew serverConfig socket quitSignal


-- | Process messages from a client and parse them for the update loop

processSocket :: ServerConfig -> Client -> MVar Bool -> IO ()
processSocket serverConfig client quitSignal = do
  -- Read from socket
  let sock = client ^. connInfo ^. socket
  msg <- NS.recv sock 256
  let zeroLength = BS.length msg == 0
  
  -- 0-length message means the other end closed their socket
  when zeroLength $ do
    let cid = client ^. clientId
    serverConfig ^. clientDisconnected $ cid

  -- If the message is valid process it
  unless zeroLength $ do
    -- A function to tokenise a bytestring
    let tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
          where (h,t) = BS.breakSubstring x y

    -- Break message up into CRLF divided packets
    let packets = tokenise "\r\n" msg

    -- Some debug output
    let cid = client ^. clientId
    forM_ packets $ \p -> unless (BS.null p) $ do
      (serverConfig ^. clientMessage) cid p

    -- Check if quitting
    quitting <- readMVar quitSignal

    -- Loop
    unless quitting $ processSocket serverConfig client quitSignal

