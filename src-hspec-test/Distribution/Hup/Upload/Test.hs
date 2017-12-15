{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{- | Support for testing  Distribution.Hup.Upload 

-}

module Distribution.Hup.Upload.Test where

import qualified Data.ByteString.Lazy.Char8 as LBS   --   pack
import qualified Network.HTTP.Client as HTTP.Client
import System.FilePath              ( (</>) )
import System.IO.Temp               (withSystemTempDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic      (run, assert, monadicIO)

import Distribution.Hup.Upload  
import Distribution.Hup.Parse.Test

type ParsedTgz = Either String (IsDocumentation, Package) 

arbAuth =
  mkAuth <$> arbitrary <*> arbitrary


-- | Round-trips an http request to check things seem to be going to the
-- right URLs.
--
-- Doesn't check the file/body, just metadata.
httpRoundTripsOK'
  :: (HTTP.Client.Request -> IO Response)
     -> Int -> Property
httpRoundTripsOK' sendRequest port = 
  forAll arbUpload $ \upl ->
    forAll arbAuth $ \auth ->
      httpRoundTripsOK  sendRequest port upl auth


httpRoundTripsOK sendRequest port upl auth = 
      monadicIO $ do
        response <- run $ emptyFileRequest port upl auth
        assert $ statusCode response == 200

        let bod = LBS.unpack $ responseBody response
            _unserialized :: (IsDocumentation, IsCandidate, ParsedTgz)
            _unserialized@(recdIsDoc1, recdIsCand, parsedTgz) = read bod

        let sentIsCand = isCandidate upl
            sentIsDoc  = uploadType  upl
            sentPkg    = package     upl 

        assert (parsedTgz == Right (sentIsDoc, sentPkg) )
        assert (sentIsCand == recdIsCand)
        assert (sentIsDoc  == recdIsDoc1)
  where
  emptyFileRequest :: Int -> Upload -> Maybe Auth -> IO Response
  emptyFileRequest port upl auth = 
    withSystemTempDirectory "huptest" $ \tmpDir -> do
      let newFile = tmpDir </> fileToUpload upl
      upl <- return $ upl { fileToUpload = newFile } 
      writeFile (tmpDir </> fileToUpload upl) ""
      let url = "http://localhost:" ++ show port ++ "/"
      buildRequest url upl auth >>= sendRequest


-- | Round-trips an http request to check things seem to be going to the
-- right URLs.
--
-- Doesn't check the file/body, just metadata.
badUrlReturns'
  :: (HTTP.Client.Request -> IO Response)
     -> Int -> Property
badUrlReturns' sendRequest port = 
  forAll arbUpload $ \upl ->
    forAll arbAuth $ \auth ->
      badUrlReturns sendRequest port upl auth



-- | Given a bad url, the http library should return a 
-- non-2XX status code, rather than throwing an exception.
badUrlReturns sendRequest port upl auth = 
  monadicIO $ do
    response <- run $ badRequest port upl auth
    assert $ statusCode response /= 200

  where
  badRequest :: Int -> Upload -> Maybe Auth -> IO Response
  badRequest port upl auth = 
    withSystemTempDirectory "huptest" $ \tmpDir -> do
      let newFile = tmpDir </> fileToUpload upl
      upl <- return $ upl { fileToUpload = newFile } 
      writeFile (tmpDir </> fileToUpload upl) ""
      let url = "http://localhost:" ++ show port ++ "/fubar/"
      buildRequest url upl auth >>= sendRequest




