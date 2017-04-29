{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Elysian.Network
  ( startListening
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
                , _newMessages :: MVar (V.Vector BS.ByteString)
                }
makeLenses ''Client


-- | The server state (connected clients etc)

data ServerState = ServerState
                     { _clientList :: [Client]
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

startListening :: ServerConfig -> IO ()
startListening serverConfig = NS.withSocketsDo $ do
  -- New clients list mvar
  quitSignal <- newMVar False
  newClients <- newMVar []

  -- Set up listening socket
  let port = serverConfig ^. listenPort
  listenSock <- N.listenOn $ port
  forkIO $ listenForNew serverConfig listenSock newClients quitSignal

  -- Set up main loop for connected clients
  let defaultServerState = ServerState []
  let updateLoop = runStateT (updateClients serverConfig newClients quitSignal) defaultServerState
  forkIO $ void updateLoop

  -- Command loop
  let loop = do
      a <- getLine
      case a of
           "q" -> modifyMVar_ quitSignal (return . const True)
           _      -> return ()
      quitting <- readMVar quitSignal
      unless quitting loop
  loop

  -- Clean up if exiting
  -- TODO: should probably check that listenForNew and updateClients threads exited
  -- before exiting
  NS.close listenSock


-- | Listen for new clients on a listening socket, adding them to a new clients list

listenForNew :: ServerConfig -> NS.Socket -> MVar [Client] -> MVar Bool -> IO ()
listenForNew serverConfig socket newClients quitSignal = do
  -- Wait for a new connection
  (sock, sockAddr) <- NS.accept socket
  let connInfo = ConnInfo sock sockAddr
  msgQueue <- newMVar V.empty
  let clientId = ClientId $ clientHash (BSC.pack . show $ sockAddr)
  let client = Client connInfo clientId msgQueue

  -- Fork off a thread to process new messages
  forkIO $ processSocket serverConfig client quitSignal

  -- Add to new clients list
  modifyMVar_ newClients $ return . (client:)

  -- Check if quitting
  quitting <- readMVar quitSignal

  -- Loop
  unless quitting $ listenForNew serverConfig socket newClients quitSignal


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
    let addr = client ^. connInfo ^. sockAddr
    forM_ packets $ \p -> unless (BS.null p) $ do
      let newMsgs = client ^. newMessages
      putStrLn $ "got message from client " ++ show addr ++ ": " ++ (show . BSC.unpack $ p)
      modifyMVar_ newMsgs (\v -> return $ V.snoc v p)

    -- Check if quitting
    quitting <- readMVar quitSignal

    -- Loop
    unless quitting $ processSocket serverConfig client quitSignal


-- | Main loop for connected clients

updateClients :: ServerConfig -> MVar [Client] -> MVar Bool -> StateT ServerState IO ()
updateClients serverConfig newClientList quitSignal = do
  -- Check for new clients
  newClients <- liftIO $ takeMVar newClientList
  liftIO $ putMVar newClientList []
  forM_ newClients $ \client ->
      liftIO $ serverConfig ^. clientConnected $ client ^. clientId

  -- Add new clients
  modifying clientList (++newClients)

  -- Process clients
  clients <- use clientList
  forM_ clients $ \client -> do
    -- Get new messages and process them
    msgQueue <- liftIO $ takeMVar (client ^. newMessages)
    liftIO $ putMVar (client ^. newMessages) V.empty

    forM_ msgQueue $ \msg -> do
      liftIO $ putStrLn $ "Got message from client: " ++ BSC.unpack msg

  -- Delay 50ms
  liftIO $ threadDelay 50000

  -- Loop
  quitting <- liftIO $ readMVar quitSignal
  unless quitting $ updateClients serverConfig newClientList quitSignal
