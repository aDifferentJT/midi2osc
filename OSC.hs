module OSC
  ( setOSCValue
  , getOSCValueAsync
  , getOSCValueSync
  , OSCConnection
  , openOSCConnection
  ) where

import Control.Concurrent (forkIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Sound.OSC as OSC (Datum (Int32, Int64, Float, Double))
import Sound.OSC (UDP, openUDP, Message (Message), packet_to_message, Datum)
import Sound.OSC (withTransport, sendMessage, waitFor)

newtype OSCConnection = OSCConnection (IO UDP)

sendOSC :: MonadIO m => OSCConnection -> Message -> m ()
sendOSC (OSCConnection udp) msg = liftIO $ withTransport udp (sendMessage msg)

openOSCConnection :: (String, Int) -> OSCConnection
openOSCConnection = OSCConnection . uncurry openUDP
--oscUDP = openUDP "192.168.1.1" 10024
--oscUDP = openUDP "169.254.147.198" 10024

setOSCValue :: MonadIO m => OSCConnection -> String -> Float -> m ()
setOSCValue conn path = sendOSC conn . Message path . (:[]) . OSC.Float

getOSCValueAsync :: OSCConnection -> String -> (Float -> IO ()) -> IO ()
getOSCValueAsync conn path callback = (>> return ()) . forkIO $ (getOSCValueSync conn path >>= callback)

getOSCValueSync :: OSCConnection -> String -> IO Float
getOSCValueSync (OSCConnection udp) path = 
  liftIO (withTransport udp (do
    sendMessage $ Message path []
    payload <- waitFor (getDataForPath path <=<  packet_to_message)
    return $ floatFromDatum (head payload)
    ))

getDataForPath :: String -> Message -> Maybe [Datum]
getDataForPath path (Message path' payload)
  | path == path' = Just payload
  | otherwise     = Nothing

floatFromDatum :: Datum -> Float
floatFromDatum (OSC.Int32  v) = fromIntegral v
floatFromDatum (OSC.Int64  v) = fromIntegral v
floatFromDatum (OSC.Float  v) = v
floatFromDatum (OSC.Double v) = realToFrac v
floatFromDatum  _             = error "OSC message doesn't contain a number"

