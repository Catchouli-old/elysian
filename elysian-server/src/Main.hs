{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.State.Strict
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network as N
import qualified Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as NS


data ConnInfo = ConnInfo
                  { _socket :: NS.Socket
                  , _sockAddr :: NS.SockAddr
                  }
makeLenses ''ConnInfo


data Client = Client
                { _connInfo :: ConnInfo
                , _newMessages :: MVar (V.Vector BS.ByteString)
                , _count :: Int
                }
makeLenses ''Client


data ServerState = ServerState
                     { _clientList :: [Client]
                     }
makeLenses ''ServerState


-- | Start the listening thread, the main thread, and the command listener

main :: IO ()
main = NS.withSocketsDo $ do
  -- New clients list mvar
  quitSignal <- newMVar False
  newClients <- newMVar []

  -- Set up listening socket
  let port = 21000
  listenSock <- N.listenOn $ N.PortNumber port
  putStrLn $ "Listening on " ++ show port
  forkIO $ listenForNew listenSock newClients quitSignal

  -- Set up main loop for connected clients
  let defaultServerState = ServerState []
  let updateLoop = runStateT (updateClients newClients quitSignal) defaultServerState
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

listenForNew :: NS.Socket -> MVar [Client] -> MVar Bool -> IO ()
listenForNew socket newClients quitSignal = do
  -- Wait for a new connection
  (sock, sockAddr) <- NS.accept socket
  putStrLn $ "Got new connection from " ++ show sockAddr
  let connInfo = ConnInfo sock sockAddr
  msgQueue <- newMVar V.empty
  let client = Client connInfo msgQueue 0

  -- Fork off a thread to process new messages
  forkIO $ processSocket client quitSignal

  -- Add to new clients list
  modifyMVar_ newClients $ return . (client:)

  -- Check if quitting
  quitting <- readMVar quitSignal

  -- Loop
  unless quitting $ listenForNew socket newClients quitSignal


-- | Process messages from a client and parse them for the update loop

processSocket :: Client -> MVar Bool -> IO ()
processSocket client quitSignal = do
  -- Read from socket
  let sock = client ^. connInfo ^. socket
  msg <- NS.recv sock 256
  let zeroLength = BS.length msg == 0
  
  -- 0-length message means the other end closed their socket
  when zeroLength $ do
    let addr = client ^. connInfo ^. sockAddr
    putStrLn $ "Client disconnected: " ++ show addr

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
    unless quitting $ processSocket client quitSignal


-- | Main loop for connected clients

updateClients :: MVar [Client] -> MVar Bool -> StateT ServerState IO ()
updateClients newClientList quitSignal = do
  -- Check for new clients
  newClients <- liftIO $ takeMVar newClientList
  liftIO $ putMVar newClientList []
  forM_ newClients $ \client -> let (ConnInfo _ sockAddr) = _connInfo client in
      liftIO . putStrLn $ "New client connecting: " ++ show sockAddr

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
  unless quitting $ updateClients newClientList quitSignal
