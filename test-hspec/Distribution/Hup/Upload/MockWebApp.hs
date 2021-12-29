{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hup.Upload.MockWebApp
  (
    webApp
  )
  where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import Network.Wai                            (Application )
import Network.Wai.Parse as Parse             (FileInfo(..), fileName)
import Web.Scotty hiding (request)

import Distribution.Hup.Upload
import Distribution.Hup.Parse

webApp :: IO Application
webApp =
    scottyApp $ do
      post "/packages/" $
        files >>= handlePost NormalPkg
      post "/packages/candidates/" $
        files >>= handlePost CandidatePkg
      put "/package/:pkgVer/docs" $ do
        pkgVer <- param "pkgVer"
        handlePut NormalPkg pkgVer
      put "/package/:pkgVer/candidate/docs" $ do
        pkgVer <- param "pkgVer"
        handlePut CandidatePkg pkgVer
  where
    handlePost
      :: IsCandidate -> [File] -> ActionM ()
    handlePost isCand files =
      case files of
        [(_fname, body)]  -> do
          let
              filename = BS.unpack $ Parse.fileName body

              parsed :: Either String (IsDocumentation, Package)
              parsed = parseTgzFilename' filename
          text $ TL.pack $ show (IsPackage, isCand, parsed)
        _       -> raise "posted package should have exactly 1 file"

    handlePut :: IsCandidate -> FilePath -> ActionM ()
    handlePut isCand filename = do
      let parsed :: Either String (IsDocumentation, Package)
          parsed = parseTgzFilename' (filename ++ "-docs.tar.gz")
      text $ TL.pack $
                show (IsDocumentation, isCand, parsed)



