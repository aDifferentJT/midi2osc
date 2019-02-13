module OSC (setOSCValue, getOSCValue, oscUDP) where

import Control.Concurrent (forkIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Sound.OSC as OSC (Datum (Int32, Int64, Float, Double))
import Sound.OSC (UDP, openUDP, Message (Message), packet_to_message, Datum)
import Sound.OSC.Transport.FD (withTransport, sendMessage, waitFor)

sendOSC :: MonadIO m => Message -> m ()
sendOSC msg = liftIO $ withTransport oscUDP (\fd -> sendMessage fd msg)

oscUDP :: IO UDP
--oscUDP = openUDP "192.168.1.1" 10024
oscUDP = openUDP "169.254.147.198" 10024

setOSCValue :: MonadIO m => String -> Float -> m ()
setOSCValue path = sendOSC . Message path . (:[]) . OSC.Float

getOSCValue :: String -> (Float -> IO ()) -> IO ()
getOSCValue path callback = (>> return ()) . forkIO $ (
  liftIO (withTransport oscUDP (\fd -> do
    sendMessage fd $ Message path []
    payload <- waitFor fd (getDataForPath path <=<  packet_to_message)
    return $ floatFromDatum (head payload)
    )) >>= callback
    )

getDataForPath :: String -> Message -> Maybe [Datum]
getDataForPath path (Message path' payload)
  | path == path' = Just payload
  | otherwise     = Nothing

floatFromDatum :: Datum -> Float
floatFromDatum (OSC.Int32 v) = fromIntegral v
floatFromDatum (OSC.Int64 v) = fromIntegral v
floatFromDatum (OSC.Float v) = v
floatFromDatum (OSC.Double v) = realToFrac v

