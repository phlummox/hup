
{-# LANGUAGE OverloadedStrings #-}


module Upload (
  doUpload
) where

import Control.Monad                      
import Control.Monad.Trans.Except         (runExcept,throwE)
import Data.ByteString.Lazy.Char8         (unpack)
import qualified Data.ByteString.Lazy as BS 
import Data.ByteString.Lazy               (ByteString)
import Data.List                          (maximumBy)
import Data.Ord                           (comparing)
import Text.HTML.TagSoup                  (parseTags, Tag(..),innerText, (~/=))

import Distribution.Hup.Upload            (Upload(..), Response(..), Auth(..)
                                          ,buildRequest, sendRequest)
import Distribution.Hup.Parse             (rstrip, lstrip,takeWhileEnd ) 

-- | do an upload.
doUpload :: String -> Upload -> Maybe Auth -> IO (Either String String)
doUpload server upl userAuth = do
  req <- buildRequest server upl userAuth
  maybeResp <- sendRequest req
  case maybeResp of
    Left exception -> return $ Left $ 
      "Request failed with a network exception: " ++ show exception
    Right resp -> return $ displayResponse resp


-- | Turn a 'Response' into some sort of hopefully useful error message
-- if it wasn't successful. 
--
-- TODO: give option of displaying successfully returned html,
-- if verbose, perhaps
displayResponse :: Response -> Either String String
displayResponse resp = runExcept $ do
  let (Response code mesg ctype body) = resp
      codeIsBad = code < 200 || code >= 300
      bodyMesg = case () of 
        _ | "text/html" `BS.isPrefixOf` ctype  -> unwords ["probable html body"
                                                  ,"is:\n", probableBody body]
          | "text/plain" `BS.isPrefixOf` ctype -> unwords ["text body is:\n"
                                                  , unpack body]
          | otherwise                          -> unwords ["body was:\n"
                                                  , show body]
  when codeIsBad $ 
       throwE $ "Request failed, status code was " ++ show code 
          ++ "status message was: "  ++ unpack mesg  
          ++ ", " ++ bodyMesg 
  -- else code is good ...
  return $ unwords ["Request succeeded with status code", show code
                    , "status message:", unpack mesg] -- , bodyMesg]

-- | drop blank lines, and collapse spaces within a line
collapseWhitespace :: String -> String
collapseWhitespace s =
  let ls = lines s
      wordsAndBack = unwords . words
  in unlines $ filter (not . null) $ map wordsAndBack ls


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

      probBod = maximumBy (comparing length) [notHeaderBits, possBodyBits]

  in collapseWhitespace probBod




