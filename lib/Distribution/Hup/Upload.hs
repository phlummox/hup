
{-# LANGUAGE OverloadedStrings #-}

{-| 

Handle uploading to a hackage server.

-}

module Distribution.Hup.Upload (
    module Distribution.Hup.Upload 
  , module Distribution.Hup.Types
  , Auth(..)
)
where

import Control.Exception (SomeException(..))
import Control.Monad              --(when)
import Control.Monad.IO.Class     --( MonadIO(..) )
import Control.Monad.Trans.Except -- (ExceptT(..),runExceptT,throwE)
import Control.Lens 
import qualified Data.List as L
import Data.List                  (dropWhileEnd)
import Data.ByteString.Char8  (pack, unpack, putStrLn)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy   as BS 
import Data.ByteString.Lazy   ( ByteString(..))
import Data.Monoid            ( (<>) )
import Data.Ord   -- comparing
import Network.Wreq as W 
import Network.Wreq.Types     hiding (auth,checkStatus)

import System.IO.Unsafe


import Distribution.Hup.Types 
  -- IsCandidate(..), IsDocumentation(..), Package(..)
import Distribution.Hup.Parse -- takeWhileEnd

import Text.HTML.TagSoup as TS

-- | Bundle together information useful for an upload.
data Upload = Upload { package :: Package -- ^ package name & version
                      ,fileToUpload  :: FilePath        -- ^ file being uploaded
                      ,uploadType    :: IsDocumentation -- ^ docco or package 
                      ,isCandidate   :: IsCandidate    -- ^ candidate or not
                     }
  deriving (Show, Eq)

-- | returns appropriate options to use with
-- a request.
-- 
-- We try to request plain text where possible; 
-- and we allow non-success statuses to still return normally
-- (rather than throwing an exception)
getOptions :: Maybe Auth -> Options
getOptions maybeAuth = defaults 
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
getUploadUrl server upload  = 
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let 
       -- we are permissive, and drop any extra trailing slashes on server.
       serverUrl = (\x -> x ++ "/") $ dropWhileEnd (=='/') $ server 

       (Upload (Package pkgName pkgVer) filePath uploadType pkgType ) = 
                                          upload
   in case uploadType of
       IsPackage -> 
          let url = case pkgType of
                         NormalPkg    -> server <>"packages/"   
                         CandidatePkg -> server <>"packages/candidates/"
          in  url 
       IsDocumentation ->
          let url = case pkgType of
                         NormalPkg    -> server <> "package/" <> pkgName
                                                <> "-" <> pkgVer <> "/docs"
   
                         CandidatePkg -> server <> "package/" <> pkgName 
                                                <> "-" <> pkgVer 
                                                <> "/candidate/docs"
          in  url 


doUpload_O
  :: String -> Upload -> Maybe Auth -> IO (Response ByteString)
doUpload_O server upload userAuth = 
-- TODO:
-- handle Yackage as well?
--  https://github.com/snoyberg/yackage/blob/master/yackage-upload.hs 
   let 
       (Upload (Package pkgName pkgVer) filePath uploadType pkgType ) = 
                                          upload
   in case uploadType of
       IsPackage -> 
          let url = getUploadUrl server upload
          in  postPkg url filePath userAuth
       IsDocumentation ->
          let url = getUploadUrl server upload
          in  putDocs url filePath userAuth

-- | try and grab what's probably the body of an html page &
-- extract the text. Our rule of thumb is, it's the bigger of the set of tags
-- coming from end to beginning that aren't obviously headers,
-- OR the tag labelled as body.
--
-- (some 404 pages don't bother to include a "body" tag)
probableBody :: Response ByteString -> String
probableBody serverResponse =
  let bod = serverResponse ^. responseBody
      parsedBod = TS.parseTags bod

      toString :: [Tag ByteString] -> String
      toString = rstrip . lstrip . LC8.unpack . innerText 

      headerTags :: [String]
      headerTags = ["<style>", "<header>", "<title>", "<meta>"]
  
      bodyTag :: String
      bodyTag = "<body>"

      notHeader t = L.all (t ~/=) headerTags  
  
      notHeaderBits = toString $ L.tail $ takeWhileEnd notHeader parsedBod

      possBodyBits = toString $ dropWhile (~/= bodyTag) parsedBod

  in L.maximumBy (comparing length) [notHeaderBits, possBodyBits]

-- | do an upload.
doUpload
  :: MonadIO m =>
     String
     -> Upload -> Maybe Auth -> m (Either String (Response ByteString))
doUpload server upload userAuth = runExceptT $ do
  resp <- liftIO $ doUpload_O server upload userAuth 
  let code  = resp ^. responseStatus ^. statusCode
      mesg  = resp ^. responseStatus ^. statusMessage
      ctype = resp ^. responseHeader "Content-Type"
      codeIsBad = code < 200 || code >= 300

  when (codeIsBad && ("text/html" `C8.isPrefixOf` ctype)) $ 
      throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
               ", probable body is:\n" ++ probableBody resp

  when (codeIsBad && ("text/plain" `C8.isPrefixOf` ctype)) $ 
      throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
               ", body is:\n" ++ LC8.unpack (resp ^. responseBody)

  when codeIsBad $
      throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
               ", full server response was:\n" ++ (show resp)
  return resp 

--test :: IO (Response ByteString)
test :: IO (Either String (Response ByteString))
test = do 
  let u = Upload (Package "foo" "0.1.0.0") "./hup-0.1.0.0.tar.gz"  IsDocumentation CandidatePkg 
  doUpload "http://www.google.com.au" u Nothing


-- either the response will throw an error, or, if we're still here,
-- it succeeded.
--
-- the response body has any warnings from the hackage server.
-- so do a catch, or ...
--                            return $ response fileFormPart^. responseBody
postPkg
  :: String -> FilePath -> Maybe Auth -> IO (Response ByteString)
postPkg url fileName userAuth = 
  let opts = getOptions userAuth 
  in  postWith opts url (partFile "package" fileName)


putDocs
  :: String
     -> FilePath -> Maybe Auth -> IO (Response ByteString)
putDocs url fileName userAuth = do 
  let opts = getOptions userAuth 
              & header "Content-Type" .~ ["application/x-tar"] 
              & header "Content-Encoding" .~ ["gzip"] 
  conts <- BS.readFile fileName 
  putWith opts url conts 


testUpload :: IO (Response ByteString)
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
    upload = Upload (Package pkgName pkgVer) fileName  uploadType pkgType 
    auth = Just $ BasicAuth (pack myUser) (pack myPassword)
  doUpload_O myServer upload auth



