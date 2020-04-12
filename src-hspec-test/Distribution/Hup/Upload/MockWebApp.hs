{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hup.Upload.MockWebApp
  (
    webApp
  )
  where


import Control.Exception                      (throwIO)
import Control.Monad
import Control.Monad.IO.Class                 (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS   --   pack
import Data.Maybe                             (fromJust)
import Network.HTTP.Types as T                (StdMethod(..))
import Network.Wai.Parse as Parse             (FileInfo(..), fileName)
import Web.Simple (ok, respond,Controller, ControllerT, routePattern, queryParam, routeMethod,parseForm, request, rawPathInfo,controllerApp, Application)

import Distribution.Hup.Upload
import Distribution.Hup.Parse

-- | We send the parsed results back as plain text,
-- parseable by 'Text.Read.read'.
webApp :: Application
webApp = controllerApp () $ do
    myReq <- request
    routeMethod T.POST $ do path <- rawPathInfo <$> request
                            let isCand = if "/candidates/" `BS.isSuffixOf`path
                                         then CandidatePkg
                                         else NormalPkg
                            when (path == "/packages/" ||
                                 path == "/packages/candidates/") $ do
                                    (_params, files) <- parseForm
                                    handlePost isCand files
    routeMethod T.PUT $ routePattern "/package/:pkgVer/:isCand" $ do
                            pkgVer <- fromJust <$> queryParam "pkgVer"
                            let filename = pkgVer :: String
                            isCand <- fromJust <$> queryParam "isCand"
                            let isCand' = if ("candidate" :: String) == isCand
                                         then CandidatePkg
                                         else NormalPkg
                            let remainingBit = if "candidate"== isCand
                                          then "docs"
                                          else ""
                            routePattern remainingBit $
                              handlePut isCand' filename
  where
    handlePost :: IsCandidate -> [(a, FileInfo c)] -> ControllerT s IO b
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

-- duplicated code from Hup.UploadSpec, remove
-- or refactor
ioAssert :: MonadIO f => Bool -> String -> f ()
ioAssert pred mesg =
  unless pred $
        liftIO $ throwIO $ userError mesg



