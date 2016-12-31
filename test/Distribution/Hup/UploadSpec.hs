{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Distribution.Hup.UploadSpec where

import Test.Hspec

import Control.Concurrent                     (forkIO, ThreadId)
import Control.Exception                      (throwIO)
import Control.Monad
import Control.Monad.IO.Class                 (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as LBS      (pack)
import Data.Maybe                             (fromJust)
import qualified Network.Socket as Soc        (Socket, close)
import Network.Wai.Handler.Warp               (Port, defaultSettings
                                              ,runSettingsSocket)

import Network.Wai.Parse as Parse             (FileInfo(..))
import Web.Frank                              (post, put)
import Web.Simple                             (Application, ControllerT
                                              ,Controller, controllerApp, ok
                                              ,parseForm, queryParam, respond)

import Distribution.Hup.Upload  
import Distribution.Hup.Parse 

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

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = 
  hspec spec 

ioAssert pred mesg = 
  unless pred $
        liftIO $ throwIO $ userError mesg

-- | We send the parsed results back as plain text,
-- parseable by 'Text.Read.read'.
webApp :: Application
webApp = controllerApp () $ do
    post "/packages/" $ do
      (_params, files) <- parseForm
      handlePost NormalPkg files
    post "/packages/candidates/" $ do
      (_params, files) <- parseForm
      handlePost CandidatePkg files
    put "/package/:pkgVer/docs" $ do
      pkgVer <- fromJust <$> queryParam "pkgVer"  
      let filename = pkgVer :: String
      handlePut NormalPkg filename
    put "/package/:pkgVer/candidate/docs" $ do
      pkgVer <- fromJust <$> queryParam "pkgVer"  
      let filename = pkgVer :: String
      handlePut CandidatePkg filename

  where 
    handlePost :: IsCandidate -> [(a, Parse.FileInfo c)] -> ControllerT s IO b
    handlePost isCand files = do
      ioAssert (length files == 1)
               "posted package should have exactly 1 file" 
      let filename = BS.unpack $ Parse.fileName $ snd $ head files
          parsed :: Either String (IsDocumentation, Package)  
          parsed = parseTgzFilename' filename
      respond $ ok "text/plain" $ LBS.pack $ show (IsPackage, isCand, parsed)

    handlePut :: IsCandidate -> FilePath -> Controller s a
    handlePut isCand filename = do
      let parsed :: Either String (IsDocumentation, Package)  
          parsed = parseTgzFilename' (filename ++ "-docs.tar.gz")
      respond $ ok "text/plain" $ LBS.pack $ 
                show (IsDocumentation, isCand, parsed)


startServer :: IO (Port, Soc.Socket, ThreadId)
startServer = do
  (port, sock) <- openFreePort 
  tid <- forkIO $ runSettingsSocket defaultSettings sock webApp
  return (port, sock, tid) 

shutdownServer :: (Port, Soc.Socket, ThreadId) -> IO ()
shutdownServer (_port, sock, _tid) = 
  Soc.close sock


spec :: Spec
spec = 
  beforeAll startServer $ afterAll shutdownServer $ 
    describe "buildRequest" $ do
      context "when its result is fed into sendRequest" $ 
        it "should send to the right web app path" $ \(port, sock, tid) -> 
          httpRoundTripsOK' port 

      context "when given a bad URL" $ 
        it "should not throw an exception" $ \(port, sock, tid) -> 
          badUrlReturns' port 







