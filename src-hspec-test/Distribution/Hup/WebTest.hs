{-# LANGUAGE CPP #-}

module Distribution.Hup.WebTest where


import Network.Wai                            (Application)
import Test.Hspec
import Test.Hspec.Core.QuickCheck             (modifyMaxSuccess)

import Distribution.Hup.Upload                (sendRequest)
import qualified Distribution.Hup.Upload as U
import Distribution.Hup.Upload.Test

#ifdef WEB_TESTS

import Control.Concurrent                     (forkIO, ThreadId)
import qualified Network.Socket as Soc        (Socket, close)
import Network.HTTP.Client
import Network.Wai.Handler.Warp               (Port, defaultSettings
                                              ,runSettingsSocket)

#if MIN_VERSION_warp(3,2,4)
import qualified Network.Wai.Handler.Warp as Warp  (openFreePort)

openFreePort :: IO (Port, Soc.Socket)
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

{-# ANN module "HLint: ignore Redundant do" #-}

-- Pulls a "Right" value out of an Either value.  If the Either value is
-- Left, raises an exception with "error".
forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x

startServer :: Application -> IO (Port, Soc.Socket, ThreadId)
startServer webApp = do
  (port, sock) <- openFreePort
  tid <- forkIO $ runSettingsSocket defaultSettings sock webApp
  return (port, sock, tid)

startServer' :: IO Application -> IO (Port, Soc.Socket, ThreadId)
startServer' webApp = do
  webApp' <- webApp
  (port, sock) <- openFreePort
  tid <- forkIO $ runSettingsSocket defaultSettings sock webApp'
  return (port, sock, tid)



shutdownServer :: (Port, Soc.Socket, ThreadId) -> IO ()
shutdownServer (_port, sock, _tid) =
  Soc.close sock

--- ?? what exactly is this testing???
liveTest :: IO Application -> SpecWith ()
liveTest webApp = do
    beforeAll (startServer' webApp) $ afterAll shutdownServer $
      describe "buildRequest" $ do
        context "when its result is fed into sendRequest" $
          modifyMaxSuccess (const 50) $
            it "should send to the right web app path" $ \(port, _sock, _tid) ->
              httpRoundTripsOK' sendRequest' port

        context "when given a bad URL" $
          modifyMaxSuccess (const 50) $
            it "should not throw an exception" $ \(port, _sock, _tid) ->
              badUrlReturns' sendRequest' port

sendRequest' :: Request -> IO U.Response
sendRequest' req = forceEither <$> sendRequest req


#else

liveTest :: Application -> SpecWith ()
liveTest _ = return ()

#endif



