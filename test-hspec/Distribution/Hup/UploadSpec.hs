{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hup.UploadSpec where

import Control.Exception                      (throwIO)
import Control.Monad
import Control.Monad.IO.Class                 (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS   --   pack
import Data.Maybe                             (fromJust)
import Data.Monoid                            ( (<>) )
import Network.HTTP.Client.MultipartFormData  (renderParts,webkitBoundary)
import Network.HTTP.Types as T                (statusCode,methodPost)
import Network.Wai.Test                       (simpleStatus,SResponse
                                              ,simpleBody, runSession)

import Test.Hspec
import Test.Hspec.Wai.Internal                (WaiSession,runWaiSession,
                                               getApp)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Distribution.Hup.Parse.Test            ( arbUpload )
import Distribution.Hup.Upload                (mkPart, bodyToByteString,
                                                getUploadUrl, Package(..),
                                                IsDocumentation(..),
                                                Upload(..), IsCandidate(..)
                                              )

import Distribution.Hup.Upload.MockWebApp (webApp)

import qualified Distribution.Hup.WebTest

import Network.Wai
import qualified Network.Wai.Test as Wai
import Network.HTTP.Types as HT

{-# ANN module ("HLint: ignore Redundant do"      :: String)  #-}

type ParsedTgz = Either String (IsDocumentation, Package)


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "testing with mocked requests" $
    describe "mocked requests" $
      context "when processed by a mock hackage server" $
        it "should go to the right web app path" $
          monadicIO $ do
            webApp' <- run webApp
            return $ httpMetadataRoundtripsOK' webApp'
  describe "testing with live HTTP requests" $
    -- this will be replaced with a stub unless the WEB_TESTS macro
    -- is defined.
    --
    -- 'webApp' is a very simple web application
    -- intended to mock some of the behaviour of a
    -- hackage server.
    Distribution.Hup.WebTest.liveTest webApp

httpMetadataRoundtripsOK' :: Application -> Property
httpMetadataRoundtripsOK' webApp =
  forAll arbUpload $ \upl -> httpMetadataRoundtripsOK webApp upl

httpMetadataRoundtripsOK
  :: Application -> Upload -> Expectation
httpMetadataRoundtripsOK webApp upl = do
  upl <- return $ upl { fileConts = Just "" }
  testRequest   <- buildTestRequest "" upl
  testResponse  <- sendTestRequest webApp testRequest

  let sentIsCand = isCandidate upl
      sentIsDoc  = uploadType  upl
      sentPkg    = package     upl

  let resStatus = T.statusCode $ simpleStatus testResponse
      resBody :: String
      resBody =   LBS.unpack $ simpleBody testResponse
      _unserialized :: (IsDocumentation, IsCandidate, ParsedTgz)
      _unserialized@(recdIsDoc1, recdIsCand, parsedTgz) = read resBody

  resStatus `shouldBe` 200
  sentIsCand `shouldBe` recdIsCand
  sentIsDoc  `shouldBe` recdIsDoc1
  parsedTgz `shouldBe` Right (sentIsDoc, sentPkg)




sendTestRequest ::
  Application -> (Request, LBS.ByteString) -> IO SResponse
sendTestRequest webApp req = do
  runWaiSession (sendRequestImproved req) webApp



testPut ::
  String -> LBS.ByteString -> (Request, LBS.ByteString)
testPut url conts =
  mkPut (BS.pack url) conts

testPost ::
  String
  -> FilePath -> LBS.ByteString -> IO (Request, LBS.ByteString)
testPost url fileName fileConts = do
  boundary <- webkitBoundary
  let part    = mkPart fileName fileConts
      headers = [("Content-Type",
                    "multipart/form-data; boundary=" <> boundary)]
  body <- bodyToByteString <$> renderParts boundary [part]
  return $ mkRequest T.methodPost (BS.pack url) headers body


-- Only call when fileConts has something in it.
buildTestRequest ::
  String -> Upload -> IO (Request, LBS.ByteString)
buildTestRequest serverUrl upl  = do
  let (Upload _ filePath fileConts uploadType _pkgType ) = upl
      url = getUploadUrl serverUrl upl
  fileConts <- return (fromJust fileConts)
  case uploadType of
      IsPackage ->
         testPost url filePath fileConts
      IsDocumentation ->
         return $ testPut url fileConts

ioAssert :: MonadIO f => Bool -> String -> f ()
ioAssert pred mesg =
  unless pred $
        liftIO $ throwIO $ userError mesg

-- expose more of hspec wai's 'request' internals

mkRequest ::
  Method -> BS.ByteString -> RequestHeaders -> LBS.ByteString -> (Request, LBS.ByteString)
mkRequest method path headers body =
  let req = defaultRequest {requestMethod = method, requestHeaders = headers}
  in (Wai.setPath req path, body)

mkPut ::
  BS.ByteString -> LBS.ByteString -> (Request, LBS.ByteString)
mkPut path body = mkRequest methodPut path [] body

sendRequestImproved :: (Request, LBS.ByteString) -> WaiSession SResponse
sendRequestImproved (req, body) = getApp >>= liftIO . runSession (Wai.srequest $ Wai.SRequest req body)



