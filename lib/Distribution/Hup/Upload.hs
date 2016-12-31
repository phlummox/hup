
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune  #-}

{-| 

Handle uploading to a hackage server, using the @HTTP@ API described
in the 
<https://hackage.haskell.org/api Hackage server documentation>.

-}

module Distribution.Hup.Upload (
    module Distribution.Hup.Upload 
  , module Distribution.Hup.Types
  , Auth(..)
)
where

--import Control.Lens 
import Data.List                        (dropWhileEnd)
import Data.Maybe                       (fromJust)
import Data.ByteString.Char8            (pack,unpack,putStrLn,ByteString(..) )
import qualified Data.ByteString.Lazy.Char8 as LBS 
import qualified Data.ByteString.Lazy as L ( ByteString(..))
import Data.Monoid                      ( (<>) )

import qualified Network.HTTP.Client as C
import Network.HTTP.Client              (requestHeaders, Request, RequestBody(..)
                                        ,method, requestBody, responseHeaders
                                        ,responseStatus)
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import qualified Network.HTTP.Types as T             
import Network.HTTP.Client.MultipartFormData (formDataBody,partFileSource)

import Distribution.Hup.Types 
-- for re-export

#ifdef TESTS

import System.FilePath              ( (</>) )
import System.IO.Temp               (withSystemTempDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic      (run, assert, monadicIO)

import Distribution.Hup.Parse       (arbUpload)

#endif


-- | Alias for <https://hackage.haskell.org/package/http-client http-client's>
-- 'Network.HTTP.Client.Response' type.
type HResponse = C.Response

-- | Username and password for HTTP basic access authentication.
data Auth = Auth {  authUser     :: ByteString
                  , authPassword :: ByteString }
  deriving (Eq, Show)

-- | Options that can be applied to a Request.
-- (e.g. to add standard headers, etc.)
--
-- Can just use 'defaultOptions'.
data Options = Options (Request -> Request)
--  deriving Show



-- | returns default options to use with
-- a request.
-- 
-- We try to request plain text where possible; 
-- and we allow non-success statuses to still return normally
-- (rather than throwing an exception)
defaultOptions :: Maybe Auth -> Options
defaultOptions mAuth = 
  case mAuth of
    Nothing -> Options id
    Just (Auth user pass) -> Options $ C.applyBasicAuth user pass

  where
    modify :: Request -> Request
    modify x = x {
          requestHeaders = ("User-Agent", "haskell hup-0.1.0.0")
                           : ("Accept",   "text/plain")
                           : requestHeaders x
        }



-- | pack a name and password into an 'Auth' structure
--
-- >>> mkAuth "myname" "mypassword"
-- Just (Auth {authUser = "myname", authPassword = "mypassword"}) 
mkAuth :: String -> String -> Maybe Auth
mkAuth name password =
    Just $ Auth (pack name) (pack password)

-- | work out what URL to upload a .tgz file to. 
-- @getUploadUrl server upload@ returns a URL.
--
-- >>> getUploadUrl "http://localhost:8080/" $ Upload (Package "foo" "0.1.0.0") "./foo-0.1.0.0.tar.gz" IsDocumentation CandidatePkg
-- "http://localhost:8080/package/foo-0.1.0.0/candidate/docs"
getUploadUrl
  :: String -> Upload -> String
getUploadUrl server upl  = 
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let 
       -- we are permissive, and drop any extra trailing slashes on server.
       serverUrl = dropWhileEnd (=='/') server 
       (Upload (Package pkgName pkgVer) _filePath uploadType pkgType ) = upl
   in case uploadType of
       IsPackage -> case pkgType of
         NormalPkg       -> serverUrl <>"/packages/"   
         CandidatePkg    -> serverUrl <>"/packages/candidates/"
       IsDocumentation ->
          case pkgType of
            NormalPkg    -> serverUrl <> "/package/" <> pkgName
                                      <> "-" <> pkgVer <> "/docs"
            CandidatePkg -> serverUrl <> "/package/" <> pkgName 
                                      <> "-" <> pkgVer 
                                      <> "/candidate/docs"

-- | @buildRequest serverUrl upl userAuth@ - create an HTTP request
-- for uploading some package (details
-- packed into @upl@) to the server at @serverUrl@, using
-- the credentials in @userAuth@.
--
-- e.g. usage:
-- > let p = Package "foo" "0.1.0.0"
-- > let u = Upload p "./foo-0.1.0.0.tar.gz" IsDocumentation CandidatePkg
-- > req <- buildRequest "http://localhost:8080/" u (mkAuth "tmp" "tmp")
-- > sendRequest req
buildRequest :: String -> Upload -> Maybe Auth -> IO Request
buildRequest serverUrl upl userAuth  =
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let (Upload _ filePath uploadType _pkgType ) = upl
   in case uploadType of
       IsPackage -> 
          let url = getUploadUrl serverUrl upl
          in  postPkg url filePath userAuth
       IsDocumentation ->
          let url = getUploadUrl serverUrl upl
          in  putDocs url filePath userAuth

-- | Send an HTTP request and get the response.
-- 
-- May throw an exception on network errors etc., but not on
-- a non-2XX response (e.g. a 404).
sendRequest
  :: Request -> IO Response
sendRequest req = do
  man <- C.newManager tlsManagerSettings
  mkResponse <$> C.httpLbs req man



-- | Relevant bits of server response, packed into a record
-- for those who don't want to deal with http-clients's 
-- 'Network.HTTP.Client.Response' type.
--
-- See 'mkResponse'.
data Response = 
  Response {
      statusCode   :: Int
    , message      :: L.ByteString
    , contentType  :: L.ByteString
    , responseBody :: L.ByteString  
  }
  deriving Show

-- adapt http-client 'Network.HTTP.Client.Response' type into a 
-- 'Response'
mkResponse :: HResponse L.ByteString -> Response
mkResponse resp =  
  let   code  = (T.statusCode . responseStatus) resp
        mesg  = LBS.fromStrict $ (T.statusMessage . responseStatus) resp
        ctype = LBS.fromStrict $ fromJust $ lookup "Content-Type" $ 
                                responseHeaders resp
        body  = C.responseBody resp
  in Response code mesg ctype body


-- | Construct a @POST@ request for uploading a package.
--
-- @postPkg url fileName userAuth@ creates a request which will upload the file
-- given by @fileName@ to the URL at @url@, using the user authentication
-- @userAuth@.
postPkg
  :: String -> FilePath -> Maybe Auth -> IO Request
postPkg url fileName userAuth = do
  let (Options opt) = defaultOptions userAuth
      addBody = formDataBody [partFileSource "package" fileName] 
  opt <$> (addBody  =<< C.parseRequest url)



-- | Build a @PUT@ request to upload package documentation.
--
-- @postPkg url fileName userAuth@ creates a request which will upload the file
-- given by @fileName@ to the URL at @url@, using the user authentication
-- @userAuth@.
putDocs :: String -> FilePath -> Maybe Auth -> IO Request
putDocs url fileName userAuth = do
  conts <- LBS.readFile fileName 
  -- build up request
  let (Options opt) = defaultOptions userAuth
      addMore x = x {
          method         = "PUT"
        , requestHeaders = ("Content-Type",       "application/x-tar")
                           : ("Content-Encoding", "gzip")
                           : requestHeaders x
        , requestBody    = RequestBodyLBS conts
        }
  (addMore . opt) <$> C.parseRequest url


#ifdef TESTS


arbAuth =
  mkAuth <$> arbitrary <*> arbitrary

     

-- | Round-trips an http request to check things seem to be going to the
-- right URLs.
--
-- Doesn't check the file/body, just metadata.
httpRoundTripsOK' :: Int -> Property
httpRoundTripsOK' port = 
  forAll arbUpload $ \upl ->
    forAll arbAuth $ \auth ->
      httpRoundTripsOK port upl auth

type ParsedTgz = Either String (IsDocumentation, Package) 

httpRoundTripsOK port upl auth = 
      monadicIO $ do
        response <- run $ withSystemTempDirectory "huptest" $ \tmpDir -> do
          let newFile = tmpDir </> (fileToUpload upl)
          upl <- return $ upl { fileToUpload = newFile } 
          writeFile (tmpDir </> (fileToUpload upl)) ""
          req <- buildRequest ("http://localhost:" ++ show port ++ "/") upl auth
          resp <- sendRequest req
          return resp
        assert $ statusCode response == 200

        let bod = LBS.unpack $ responseBody response
            unserialized :: (IsDocumentation, IsCandidate, ParsedTgz)
            unserialized@(recdIsDoc1, recdIsCand, parsedTgz) = read bod

        let sentIsCand = isCandidate upl
            sentIsDoc  = uploadType  upl
            sentPkg    = package     upl 

        assert (parsedTgz == Right (sentIsDoc, sentPkg) )
        assert (sentIsCand == recdIsCand)
        assert (sentIsDoc  == recdIsDoc1)


#endif

