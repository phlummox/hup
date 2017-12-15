
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune  #-}
{-# LANGUAGE CPP #-}

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

import Control.Monad
import qualified Data.ByteString.Builder as Bu
import Data.List                        (dropWhileEnd)
import Data.Maybe                       (fromJust)
import Data.ByteString.Char8            (pack,ByteString )
import qualified Data.ByteString.Lazy.Char8 as LBS 
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Monoid                      ( (<>) )
import qualified Network.HTTP.Client as C
import Network.HTTP.Client              (requestHeaders, Request, RequestBody(..)
                                        ,method, requestBody, responseHeaders
                                        ,responseStatus)
import Network.HTTP.Client.TLS          (tlsManagerSettings)
import qualified Network.HTTP.Types as T             
import Network.HTTP.Client.MultipartFormData 
                                        (formDataBody,partFileRequestBodyM
                                        ,Part)

import Distribution.Hup.Types 
-- for re-export

import Control.Exception


#if MIN_VERSION_http_client(0,4,30)
--parseRequest :: MonadThrow m => String -> m Request
parseRequest = C.parseRequest
#else

--parseRequest :: MonadThrow m => String -> m Request
parseRequest = 
    fmap noThrow . C.parseUrl
  where
    noThrow req = req { C.checkStatus = \_ _ _ -> Nothing }
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
    Just (Auth user pass) -> Options $ modify . C.applyBasicAuth user pass

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
-- >>> getUploadUrl "http://localhost:8080/" $ Upload (Package "foo" "0.1.0.0") "./foo-0.1.0.0.tar.gz" Nothing IsDocumentation CandidatePkg
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
       (Upload (Package pkgName pkgVer) _filePath _fileConts uploadType pkgType ) = upl
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
--
-- > let p = Package "foo" "0.1.0.0"
-- > let u = Upload p "./foo-0.1.0.0.tar.gz" Nothing IsDocumentation CandidatePkg
-- > req <- buildRequest "http://localhost:8080/" u (mkAuth "tmp" "tmp")
-- > sendRequest req
buildRequest :: String -> Upload -> Maybe Auth -> IO Request
buildRequest serverUrl upl userAuth  =
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let (Upload _ filePath fileConts uploadType _pkgType ) = upl
   in case uploadType of
       IsPackage -> do
          let url = getUploadUrl serverUrl upl
          postPkg url filePath fileConts userAuth
       IsDocumentation -> do
          let url = getUploadUrl serverUrl upl
          putDocs url filePath fileConts userAuth

-- | Send an HTTP request and get the response (or an exception)
sendRequest :: Request -> IO (Either C.HttpException Response) 
sendRequest req = 
  do
    man <- C.newManager tlsManagerSettings
    tryHttp (mkResponse <$> C.httpLbs req man) 
  where
    --idHttp :: (C.HttpException -> IO a) -> (C.HttpException -> IO a) 
    --idHttp = id

    tryHttp ::  IO a -> IO (Either C.HttpException a) 
    tryHttp = try




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
-- @postPkg url conts userAuth@ creates a request which will upload the file conts
-- in @conts@ to the URL at @url@, using the user authentication
-- @userAuth@.
postPkg
  :: String -> FilePath -> Maybe L.ByteString -> Maybe Auth -> 
     IO Request
postPkg url fileName fileConts userAuth = do
  let conts :: IO RequestBody
      conts = RequestBodyLBS `liftM` 
                  maybe (LBS.readFile fileName) return fileConts
      (Options opt) = defaultOptions userAuth
      formBody = formDataBody [partFileRequestBodyM "package" fileName conts]
  opt <$> (formBody =<< parseRequest url)

-- | Build a @PUT@ request to upload package documentation.
--
-- @putDocs url fileConts userAuth@ creates a request which will upload the file contents
-- in @fileConts@ to the URL at @url@, using the user authentication
-- @userAuth@.
putDocs :: String -> FilePath -> Maybe L.ByteString -> Maybe Auth -> IO Request
putDocs url fileName fileConts userAuth = do
  conts <- maybe (LBS.readFile fileName) return fileConts
  -- build up request
  let (Options opt) = defaultOptions userAuth
      addMore x = x {
          method         = "PUT"
        , requestHeaders = ("Content-Type",       "application/x-tar")
                           : ("Content-Encoding", "gzip")
                           : requestHeaders x
        , requestBody    = RequestBodyLBS conts
        }
  (addMore . opt) <$> parseRequest url

-- | given a filename and contents, produce an http-client 'Part'
-- for uploading as a package to a hackage server
mkPart :: FilePath -> L.ByteString -> Part
mkPart fileName fileConts = do
  let myConts = return $ RequestBodyLBS fileConts
  partFileRequestBodyM "package" fileName myConts


-- | Convert a 'RequestBody' to a 'ByteString'.
-- 
-- For testing purposes. Won't work if your 'RequestBody' is set up to do 
-- streaming (e.g. using the 'RequestBodyStream' constructor/ 'partFileSource').
bodyToByteString :: RequestBody -> L.ByteString
bodyToByteString b = case b of
    RequestBodyLBS lbs             -> lbs
    RequestBodyBS bs               -> Bu.toLazyByteString $ Bu.byteString bs
    RequestBodyBuilder _sz builder -> Bu.toLazyByteString builder
    _                              -> error "bodyToBS not done yet"



