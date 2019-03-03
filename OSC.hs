{-# LANGUAGE RecordWildCards, DeriveAnyClass, ScopedTypeVariables #-}

module OSC
  ( setOSCValue
  , askForOSCValue
  , registerCallback
  , unregisterCallback
  , unregisterAllCallbacks
  , OSCConnection
  , openOSCConnection
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception, throw, try, IOException)
import Control.Monad (void, when, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Data.Map.Strict (Map, insert, delete, empty)

import qualified Sound.OSC as OSC (Datum (Int32, Int64, Float, Double))
import Sound.OSC
  ( UDP (UDP)
  , openUDP
  , Message (Message)
  , Datum
  , withTransport
  , sendMessage
  , recvMessages
  , sendTo
  , with_udp
  , Packet (Packet_Message)
  )
import Network.Socket --(AddrInfo (AddrInfo), addrAddress, getAddrInfo)
import qualified Network.Socket as N

data OSCConnection = OSCConnection (IO UDP) (IO UDP) String Int (IORef (Map String (Float -> IO ())))

data OSCException = OSCNotANumberException
  deriving (Show, Exception)

sendOSC :: MonadIO m => OSCConnection -> Message -> m ()
sendOSC (OSCConnection udp _ _ _ _) msg = liftIO $ withTransport udp (sendMessage msg)

sendOSCFromServer :: MonadIO m => OSCConnection -> Message -> m ()
sendOSCFromServer (OSCConnection _ udp a p _) m = liftIO $ do
  AddrInfo{..}:_ <- getAddrInfo Nothing (Just a) (Just . show $ p)
  either (\(e :: IOException) -> print ("Sending failed: " ++ show e)) return =<< try (with_udp udp $ \udp' -> Sound.OSC.sendTo udp' (Packet_Message m) addrAddress)

sendRegisterMessages :: OSCConnection -> String -> IO ()
sendRegisterMessages oscConn m = void . forkIO . forever $ do
  sendOSCFromServer oscConn (Message m [])
  threadDelay 9000000

-- | Variant of 'udpServer' that doesn't require the host address.
udpServer' :: Maybe String -> Int -> IO UDP
udpServer' h p = do
  let hints =
        N.defaultHints
        {N.addrFlags = [N.AI_PASSIVE,N.AI_NUMERICSERV]
        ,N.addrSocketType = N.Datagram}
  a:_ <- N.getAddrInfo (Just hints) h (Just (show p))
  s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.setSocketOption s N.ReuseAddr 1
  when (N.isSupportedSocketOption N.ReusePort) $ N.setSocketOption s N.ReusePort 1
  N.bind s (N.addrAddress a)
  return (UDP s)

openOSCConnection :: (String, Int, Int, [String]) -> IO OSCConnection
openOSCConnection (a, pO, pF, regs) = do
  callbacks <- newIORef empty
  let udpO = openUDP a pO
  let udpFI = udpServer' Nothing pF
  let udpFO = udpServer' (Just "0.0.0.0") pF
  let oscConn = OSCConnection udpO udpFO a pO callbacks
  mapM_ (sendRegisterMessages oscConn) regs
  listenToOSC callbacks udpFI
  return oscConn

--oscUDP = openUDP "192.168.1.1" 10024
--oscUDP = openUDP "169.254.147.198" 10024

setOSCValue :: MonadIO m => OSCConnection -> String -> Float -> m ()
setOSCValue conn path = sendOSC conn . Message path . (:[]) . OSC.Float

askForOSCValue :: MonadIO m => OSCConnection -> String -> m ()
askForOSCValue conn path = sendOSCFromServer conn . Message path $ []

listenToOSC :: IORef (Map String (Float -> IO ())) -> IO UDP -> IO ()
listenToOSC callbacks udp = void . forkIO . withTransport udp . forever $
  recvMessages >>= mapM_ (\(Message path datum) -> lift . runMaybeT_ $ do
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
floatFromDatum  _             = throw OSCNotANumberException
