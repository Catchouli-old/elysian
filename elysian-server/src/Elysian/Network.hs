{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Elysian.Network
  ( startListening
  , stopListening
  , isQuitting
  , getClients
  , ServerConfig(..)
  , defaultConfig
  , N.PortID(..)
  , clientHash
  , ClientId
  , Server
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
import qualified Data.Map.Strict as M
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

data ClientId = ClientId (Digest SHA1) deriving (Show, Ord, Eq)


-- | Information about a connected client

data Client = Client
                { _connInfo :: ConnInfo
                , _clientId :: ClientId
                }
makeLenses ''Client


-- | The server state (connected clients etc)

data ServerState = ServerState
                     { _quitSignal :: MVar Bool
                     , _clientList :: M.Map ClientId Client
                     , _listenSock :: NS.Socket
                     }
makeLenses ''ServerState


-- | An encapsulation for the server state that we can pass out

data Server = Server (MVar ServerState)


-- | The server config (port, callbacks etc)

data ServerConfig = ServerConfig
                      { _listenPort :: N.PortID
                      , _clientConnected :: Server -> ClientId -> IO ()
                      , _clientDisconnected :: Server -> ClientId -> IO ()
                      , _clientMessage :: Server -> ClientId -> BS.ByteString -> IO ()
                      }
makeLenses ''ServerConfig


-- | A default config that does nothing

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
                  { _listenPort = N.PortNumber 0
                  , _clientConnected = \_ _ -> return ()
                  , _clientDisconnected = \_ _ -> return ()
                  , _clientMessage = \_ _ _ -> return ()
                  }


-- | ByteString to client hash

clientHash :: BS.ByteString -> Digest SHA1
clientHash = hash


-- | Start the listening thread, the main thread, and the command listener

startListening :: ServerConfig -> IO Server
startListening serverConfig = NS.withSocketsDo $ do
  -- Signal that can be set to signal the other threads to exit
  quitSignal <- newMVar False

  -- Set up listening socket
  let port = serverConfig ^. listenPort
  listenSock <- N.listenOn $ port

  -- Server state
  let defaultServerState = ServerState quitSignal M.empty listenSock
  serverState <- newMVar defaultServerState

  -- Fork listening thread
  forkIO $ listenForNew serverConfig serverState listenSock quitSignal

  return $ Server serverState


-- | Stop listening

stopListening :: Server -> IO ()
stopListening (Server state) = do
  serverState <- readMVar state
  modifyMVar_ (serverState ^. quitSignal) (return . const True)
  NS.close (serverState ^. listenSock)


-- | Whether stopListening has been called

isQuitting :: Server -> IO Bool
isQuitting (Server state) = do
  serverState <- readMVar state
  readMVar (serverState ^. quitSignal)


-- | Get a list of clients

getClients :: Server -> IO (M.Map ClientId Client)
getClients (Server state) = do
  serverState <- readMVar state
  return $ serverState ^. clientList


-- | Listen for new clients on a listening socket, adding them to a new clients list

listenForNew :: ServerConfig -> MVar ServerState -> NS.Socket -> MVar Bool -> IO ()
listenForNew serverConfig serverState socket quitSignal = do
  let server = Server serverState

  -- Wait for a new connection
  (sock, sockAddr) <- NS.accept socket
  let connInfo = ConnInfo sock sockAddr
  let clientId = ClientId $ clientHash (BSC.pack . show $ sockAddr)
  let client = Client connInfo clientId

  -- Add new client
  modifyMVar_ serverState $ \s -> return s { _clientList = M.insert clientId client (s ^. clientList) }

  -- Fork off a thread to process new messages
  forkIO $ processSocket serverConfig serverState client quitSignal

  -- Client connected callback
  (serverConfig ^. clientConnected) server clientId

  -- Check if quitting
  quitting <- readMVar quitSignal

  -- Loop
  unless quitting $ listenForNew serverConfig serverState socket quitSignal


-- | Process messages from a client and parse them for the update loop

processSocket :: ServerConfig -> MVar ServerState -> Client -> MVar Bool -> IO ()
processSocket serverConfig serverState client quitSignal = do
  let server = Server serverState

  -- Read from socket
  let sock = client ^. connInfo ^. socket
  msg <- NS.recv sock 256
  let zeroLength = BS.length msg == 0
  
  -- 0-length message means the other end closed their socket
  when zeroLength $ do
    let cid = client ^. clientId
    (serverConfig ^. clientDisconnected) server cid
    modifyMVar_ serverState $ \s -> return s { _clientList = M.delete cid (s ^. clientList) }

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
      (serverConfig ^. clientMessage) server cid p

    -- Check if quitting
    quitting <- readMVar quitSignal

    -- Loop
    unless quitting $ processSocket serverConfig serverState client quitSignal

