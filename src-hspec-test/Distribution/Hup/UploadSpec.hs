{-# LANGUAGE OverloadedStrings #-}

module Distribution.Hup.UploadSpec where




import Control.Exception                      (throwIO)
import Control.Monad
import Control.Monad.IO.Class                 (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS   --   pack
import Data.Maybe                             (fromJust)
import Data.Monoid                            ( (<>) )
import Network.HTTP.Client.MultipartFormData  (renderParts,webkitBoundary)
import Network.HTTP.Types as T                (statusCode,methodPost
                                              ,StdMethod(..)) 
import Network.Wai.Parse as Parse             (FileInfo(..), fileName)
import Network.Wai.Test                       (simpleStatus,SResponse
                                              ,simpleBody)
import Test.Hspec
import qualified Test.Hspec.Wai as HWai       --(put, request)
import Test.Hspec.Wai.Internal                --(WaiSession,runWaiSession)
import Test.QuickCheck                        --(forAll)
import Test.QuickCheck.Monadic                --(assert, run, monadicIO )
import Web.Simple --(Application,controllerApp, ControllerT,Controller,ok,parseForm, queryParam, rawPathInfo, request, respond, routeMethod, routePattern)


import Distribution.Hup.Upload  
import Distribution.Hup.Parse 
import Distribution.Hup.Parse.Test

import qualified Distribution.Hup.WebTest 

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
        it "should go to the right web app path" 
          httpMetadataRoundtripsOK'  
  describe "testing with live HTTP requests" $ 
    -- this will be replaced with a stub unless the WEB_TESTS macro
    -- is defined.
    Distribution.Hup.WebTest.liveTest webApp



httpMetadataRoundtripsOK' = 
  forAll arbUpload $ \upl -> httpMetadataRoundtripsOK upl 


httpMetadataRoundtripsOK upl = monadicIO $ do 
  upl <- return $ upl { fileConts = Just "" }
  testRequest <- run $ buildTestRequest "" upl 
  testResponse <- run $ sendTestRequest testRequest

  let resStatus = T.statusCode $ simpleStatus testResponse
      resBody :: String
      resBody =   LBS.unpack $ simpleBody testResponse
      _unserialized :: (IsDocumentation, IsCandidate, ParsedTgz)
      _unserialized@(recdIsDoc1, recdIsCand, parsedTgz) = read resBody

  let sentIsCand = isCandidate upl
      sentIsDoc  = uploadType  upl
      sentPkg    = package     upl

  assert (resStatus == 200)
  assert (sentIsCand == recdIsCand)
  assert (sentIsDoc  == recdIsDoc1)
  assert (parsedTgz == Right (sentIsDoc, sentPkg) )


sendTestRequest :: WaiSession SResponse -> IO SResponse
sendTestRequest testReq =   
  runWaiSession testReq webApp

--showInd x =
--  indent 4 (show x) 
--
--indent n = 
--  let wspace = replicate n ' ' 
--  in  unlines .  (map (wspace ++)) . lines

testPut :: String -> LBS.ByteString -> WaiSession SResponse
testPut url conts = 
  HWai.put (BS.pack url) conts

testPost
  :: String -> FilePath -> LBS.ByteString -> IO (WaiSession SResponse)
testPost url fileName fileConts = do
  boundary <- webkitBoundary 
  let part    = mkPart fileName fileConts
      headers = [("Content-Type", 
                    "multipart/form-data; boundary=" <> boundary)]
  body <- bodyToByteString <$> renderParts boundary [part]
  return $ HWai.request T.methodPost (BS.pack url) headers body


-- Only call when fileConts has something in it.
buildTestRequest
  :: String -> Upload -> IO (WaiSession SResponse)
buildTestRequest serverUrl upl  = do
  let (Upload _ filePath fileConts uploadType _pkgType ) = upl
      url = getUploadUrl serverUrl upl
  fileConts <- return (fromJust fileConts)
  case uploadType of
      IsPackage -> 
         testPost url filePath fileConts
      IsDocumentation -> 
         return $ testPut url fileConts 


ioAssert pred mesg = 
  unless pred $
        liftIO $ throwIO $ userError mesg

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






