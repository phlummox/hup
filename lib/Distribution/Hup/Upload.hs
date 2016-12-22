
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Exception (SomeException(..))
import Control.Lens 
import Data.List                  (dropWhileEnd)
import Data.ByteString.Char8  (pack, unpack, putStrLn)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy   as BS 
import Data.ByteString.Lazy   ( ByteString(..))
import Data.Monoid            ( (<>) )
import Network.Wreq as W hiding (Response, statusCode) 
import qualified Network.Wreq as W 
import Network.Wreq.Types     hiding (auth,checkStatus,responseBody)

import Distribution.Hup.Types 
  -- IsCandidate(..), IsDocumentation(..), Package(..)
import Distribution.Hup.Parse -- takeWhileEnd

-- | Alias for <https://hackage.haskell.org/package/wreq wreq's>
-- 'Network.Wreq.Response' type.
type WResponse = W.Response


-- | Bundle together information useful for an upload.
data Upload = 
    Upload { package       :: Package -- ^ package name & version
            ,fileToUpload  :: FilePath        -- ^ file being uploaded
            ,uploadType    :: IsDocumentation -- ^ docco or package 
            ,isCandidate   :: IsCandidate    -- ^ candidate or not
           } deriving (Show, Eq)

-- | returns default options to use with
-- a request.
-- 
-- We try to request plain text where possible; 
-- and we allow non-success statuses to still return normally
-- (rather than throwing an exception)
defaultOptions :: Maybe Auth -> Options
defaultOptions maybeAuth = defaults 
                          & header "Accept" .~ ["text/plain"] 
                          & auth .~ maybeAuth
                          & checkStatus .~ (Just myHandler)
  where 
    --myHandler :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
    myHandler :: Status -> t1 -> t2 -> Maybe SomeException
    myHandler status headers jar = Nothing 

-- | pack a name and password into an 'Auth' structure
--
-- >>> mkAuth "myname" "mypassword"
mkAuth name password =
    Just $ BasicAuth (pack name) (pack password)

-- | work out what URL to upload a .tgz file to. 
-- @getUploadUrl server upload@ returns a URL.
--
-- >>> getUploadUrl "http://localhost:8080/" $ Upload (Package "foo" "0.1.0.0") "./hup-0.1.0.0.tar.gz" IsDocumentation CandidatePkg
-- "http://localhost:8080/package/foo-0.1.0.0/candidate/docs"
getUploadUrl
  :: String -> Upload -> String
getUploadUrl server upl  = 
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let 
       -- we are permissive, and drop any extra trailing slashes on server.
       serverUrl = dropWhileEnd (=='/') $ server 
       (Upload (Package pkgName pkgVer) filePath uploadType pkgType ) = upl
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

-- | @upload serverUrl upl userAuth@ - upload some package (details
-- packed into @upl@ to the server at @serverUrl@, using
-- the credentials in @userAuth@.
upload
  :: String -> Upload -> Maybe Auth -> IO (WResponse ByteString)
upload serverUrl upl userAuth = 
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


-- | Relevant bits of server response, packed into a record
-- for those who don't want to deal with wreq's 
-- 'Network.Wreq.Response' type.
data Response = 
  Response {
      statusCode   :: Int
    , message      :: ByteString
    , contentType  :: ByteString
    , responseBody :: ByteString  
  }

-- | adapt wreq's 'Network.Wreq.Response' type into a 'Response'
mkResponse :: WResponse ByteString -> Response
mkResponse resp = 
  let code  = resp ^. responseStatus ^. W.statusCode
      mesg  = BS.fromStrict $ resp ^. responseStatus ^. statusMessage
      ctype = BS.fromStrict $ resp ^. responseHeader "Content-Type"
      body  = resp ^. W.responseBody
  in Response code mesg ctype body


-- | Do a @POST@ request to upload a package.
--
-- @postPkg url fileName userAuth@ will try to upload the file given
-- by @fileName@ to the URL at @url@, using the user authentication
-- @userAuth@.
postPkg
  :: String -> FilePath -> Maybe Auth -> IO (WResponse ByteString)
postPkg url fileName userAuth = 
  let opts = defaultOptions userAuth 
  in  postWith opts url (partFile "package" fileName)


-- | Do a @PUT@ request to upload package documentation.
--
-- @postPkg url fileName userAuth@ will try to upload the file given
-- by @fileName@ to the URL at @url@, using the user authentication
-- @userAuth@.
putDocs
  :: String
     -> FilePath -> Maybe Auth -> IO (WResponse ByteString)
putDocs url fileName userAuth = do 
  let opts = defaultOptions userAuth 
              & header "Content-Type" .~ ["application/x-tar"] 
              & header "Content-Encoding" .~ ["gzip"] 
  conts <- BS.readFile fileName 
  putWith opts url conts 


testUpload :: IO (WResponse ByteString)
testUpload = do
  let
    myServer   = "http://localhost:8080" 
    myUser     = "tmp"
    myPassword = "tmp"
    pkgName    = "silly"
    pkgVer     = "0.1.0.0"
    --fileName   = pkgName <> "-" <> pkgVer <> ".tar.gz"
    fileName   = "../../silly-0.1.0.0-docs.tar.gz"
    --pkgType    = NormalPkg
    pkgType    = CandidatePkg
    --uploadType = IsPackage 
    uploadType = IsDocumentation 
    upl = Upload (Package pkgName pkgVer) fileName  uploadType pkgType 
    auth = Just $ BasicAuth (pack myUser) (pack myPassword)
  upload myServer upl auth



