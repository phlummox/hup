{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Distribution.Hup.WebTest where


import Distribution.Hup.Upload  
import Distribution.Hup.Upload.Test
import Distribution.Hup.Parse 
import Network.Wai                            (Application)
import Test.Hspec
import Test.QuickCheck

#ifdef WEB_TESTS

import Control.Concurrent                     (forkIO, ThreadId)
import qualified Network.Socket as Soc        (Socket, close)
import Network.Wai.Handler.Warp               (Port, defaultSettings
                                              ,runSettingsSocket)


#if MIN_VERSION_warp(3,2,4)
import qualified Network.Wai.Handler.Warp as Warp  (openFreePort)

openFreePort = Warp.openFreePort
#else
import Network.Socket

openFreePort :: IO (Port, Soc.Socket)
openFreePort = do 
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
#endif

startServer :: Application -> IO (Port, Soc.Socket, ThreadId)
startServer webApp = do
  (port, sock) <- openFreePort 
  tid <- forkIO $ runSettingsSocket defaultSettings sock webApp
  return (port, sock, tid) 

shutdownServer :: (Port, Soc.Socket, ThreadId) -> IO ()
shutdownServer (_port, sock, _tid) = 
  Soc.close sock

liveTest :: Application -> SpecWith ()
liveTest webApp = do
    beforeAll (startServer webApp) $ afterAll shutdownServer $ 
      describe "buildRequest" $ do
        context "when its result is fed into sendRequest" $ 
          it "should send to the right web app path" $ \(port, sock, tid) -> 
            httpRoundTripsOK' sendRequest port 

        context "when given a bad URL" $ 
          it "should not throw an exception" $ \(port, sock, tid) -> 
            badUrlReturns' sendRequest port 

#else

liveTest :: Application -> SpecWith ()
liveTest _ = return ()

#endif




