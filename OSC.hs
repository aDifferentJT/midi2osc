module OSC (sendOSC, oscUDP) where

import Control.Monad.IO.Class

import qualified Sound.OSC              as OSC
import qualified Sound.OSC.Transport.FD as OSC.FD

sendOSC :: MonadIO m => OSC.Message -> m ()
sendOSC msg = liftIO $ OSC.FD.withTransport oscUDP (\fd -> OSC.FD.sendMessage fd msg)

oscUDP :: IO OSC.UDP
oscUDP = OSC.openUDP "192.168.1.1" 10024

