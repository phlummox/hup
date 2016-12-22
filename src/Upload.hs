
{-# LANGUAGE OverloadedStrings #-}


module Upload (
  doUpload
) where

import Control.Monad                      
import Control.Monad.Trans.Except         (ExceptT(..),runExcept,throwE)
import Data.ByteString.Lazy.Char8         (unpack)
import qualified Data.ByteString.Lazy as BS 
import Data.ByteString.Lazy               (ByteString(..))
import Data.List                          (all, maximumBy)
import Data.Ord                           (comparing)
import Text.HTML.TagSoup                  (parseTags, Tag(..),innerText, (~/=))

import Distribution.Hup.Upload            (Upload(..), Response(..), Auth(..)
                                          ,upload, mkResponse)
import Distribution.Hup.Parse             (rstrip, lstrip,takeWhileEnd ) 

-- | do an upload.
doUpload :: String -> Upload -> Maybe Auth -> IO (Either String ())
doUpload server upl userAuth = 
  (displayResponse . mkResponse)  `liftM` upload server upl userAuth 

-- | Turn a 'Response' into some sort of hopefully useful error message
-- if it wasn't successful. 
displayResponse :: Response -> Either String ()
displayResponse resp = runExcept $ do
  let (Response code mesg ctype body) = resp
      codeIsBad = code < 200 || code >= 300
  when (codeIsBad && ("text/html" `BS.isPrefixOf` ctype)) $ 
       throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
                ", probable body is:\n" ++ probableBody body
  when (codeIsBad && ("text/plain" `BS.isPrefixOf` ctype)) $ 
       throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
                ", body is:\n" ++ unpack body
  when codeIsBad $
       throwE $ "Request failed, status message was: "  ++ unpack mesg ++ 
                ", server response body was:\n" ++ show body
  return ()

-- | try and grab what's probably the body of an html page &
-- extract the text. Our rule of thumb is, it's the bigger of the set of tags
-- coming from end to beginning that aren't obviously headers,
-- OR the tag labelled as body.
--
-- (some 404 pages don't bother to include a "body" tag)
probableBody :: ByteString -> String
probableBody bod = 
  let 
      toString :: [Tag ByteString] -> String
      toString = rstrip . lstrip . unpack . innerText 
      headerTags :: [String]
      headerTags = ["<style>", "<header>", "<title>", "<meta>"]
      bodyTag :: String
      bodyTag = "<body>"

      parsedBod = parseTags bod
      notHeader t = all (t ~/=) headerTags  
      notHeaderBits = toString $ tail $ takeWhileEnd notHeader parsedBod
      possBodyBits = toString $ dropWhile (~/= bodyTag) parsedBod

  in maximumBy (comparing length) [notHeaderBits, possBodyBits]



----test :: IO (Response ByteString)
--test :: IO (Either String (Response ByteString))
--test = do 
--  let u = Upload (Package "foo" "0.1.0.0") "./hup-0.1.0.0.tar.gz"  IsDocumentation CandidatePkg 
--  doUpload "http://www.google.com.au" u Nothing



