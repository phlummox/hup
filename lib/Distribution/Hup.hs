
-- {-# OPTIONS_HADDOCK hide, prune #-}

{- |
  Bits and pieces for building and uploading source or documentation .tar files
  for Hackage, intended to make it easy to write your own Haskell 
  programs/scripts for managing uploads.

  This is the main entry-point to look at, and more low-level functions are
  available in the other modules.
-}

module Distribution.Hup 
(
--  -- | module hup
  module Distribution.Hup 
-- * Finding and parsing Cabal files 
  , findCabal
  , readCabal
  , extractCabal
-- * Parsing .tgz file names
  , parseTgzFilename
  , parseTgzFilename'
-- * Building tar files
  , buildTar
-- * Uploading
  , getUploadUrl
  , mkAuth
  , postPkg
  , putDocs
  , buildRequest
  , sendRequest
-- * Types
  , IsCandidate(..)
  , IsDocumentation(..) 
  , Package(..) 
  , Upload(..)
  , Auth(..)
)
 where


import Distribution.Hup.BuildTar 
import Distribution.Hup.Parse
import Distribution.Hup.Types
import Distribution.Hup.Upload

