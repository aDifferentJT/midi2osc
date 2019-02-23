{-# LANGUAGE RecordWildCards #-}

module OSC
  ( setOSCValue
  , askForOSCValue
  , registerCallback
  , unregisterCallback
  , unregisterAllCallbacks
  , OSCConnection
  , openOSCConnection
  ) where

import Utils

import Prelude hiding (lookup)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map, lookup, insert, delete, empty)

import qualified Sound.OSC as OSC (Datum (Int32, Int64, Float, Double))
import Sound.OSC
  (UDP
  , openUDP
  , udp_server
  , udpServer
  , Message (Message)
  , Datum
  , withTransport
  , sendMessage
  , recvMessages
  , sendTo
  , with_udp
  , Packet (Packet_Message)
  )
import Network.Socket (AddrInfo (AddrInfo), addrAddress, getAddrInfo)

data OSCConnection = OSCConnection (IO UDP) (IO UDP) String Int (IORef (Map String (Float -> IO ())))

sendOSC :: MonadIO m => OSCConnection -> Message -> m ()
sendOSC (OSCConnection udp _ _ _ _) msg = liftIO $ withTransport udp (sendMessage msg)

sendOSCFromServer :: MonadIO m => OSCConnection -> Message -> m ()
sendOSCFromServer (OSCConnection _ udp a p _) m = liftIO $ do
  AddrInfo{..}:_ <- getAddrInfo Nothing (Just a) (Just . show $ p)
  with_udp udp (\udp' -> sendTo udp' (Packet_Message m) (addrAddress))

sendRegisterMessages :: OSCConnection -> String -> IO ()
sendRegisterMessages oscConn m = void . forkIO . infLoop $ do
  sendOSCFromServer oscConn (Message m [])
  threadDelay 9000000

openOSCConnection :: (String, Int, Int, [String]) -> IO OSCConnection
openOSCConnection (a, pO, pF, regs) = do
  callbacks <- newIORef empty
  let udpO = openUDP a pO
  let udpFI = udp_server pF
  let udpFO = udpServer "0.0.0.0" pF
  let oscConn = OSCConnection udpO udpFO a pO callbacks
  sequence_ (map (sendRegisterMessages oscConn) regs)
  listenToOSC callbacks udpFI
  return $ oscConn

--oscUDP = openUDP "192.168.1.1" 10024
--oscUDP = openUDP "169.254.147.198" 10024

setOSCValue :: MonadIO m => OSCConnection -> String -> Float -> m ()
setOSCValue conn path = sendOSC conn . Message path . (:[]) . OSC.Float

askForOSCValue :: MonadIO m => OSCConnection -> String -> m ()
askForOSCValue conn path = sendOSCFromServer conn . Message path $ []

listenToOSC :: IORef (Map String (Float -> IO ())) -> IO UDP -> IO ()
listenToOSC callbacks udp = void . forkIO . withTransport udp . infLoop $ do
  recvMessages >>= sequence_ . map (\(Message path datum) -> lift . runMaybeT_ $ do
    callback <- MaybeT $ lookup path <$> readIORef callbacks
    lift . callback . floatFromDatum $ datum
    )

registerCallback :: OSCConnection -> String -> (Float -> IO ()) -> IO ()
registerCallback (OSCConnection _ _ _ _ callbacks) path callback = modifyIORef' callbacks (insert path callback)

unregisterCallback :: OSCConnection -> String -> IO ()
unregisterCallback (OSCConnection _ _ _ _ callbacks) path = modifyIORef' callbacks (delete path)

unregisterAllCallbacks :: OSCConnection -> IO ()
unregisterAllCallbacks (OSCConnection _ _ _ _ callbacks) = writeIORef callbacks empty

floatFromDatum :: [Datum] -> Float
floatFromDatum [OSC.Int32  v] = fromIntegral v
floatFromDatum [OSC.Int64  v] = fromIntegral v
floatFromDatum [OSC.Float  v] = v
floatFromDatum [OSC.Double v] = realToFrac v
floatFromDatum []             = 0
floatFromDatum  _             = error "OSC message doesn't contain a number"
