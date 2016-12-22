
-- {-# OPTIONS_HADDOCK hide, prune #-}

{- |
  Functions for building and uploading .tar files for Hackage, containing 
  source or documentation.

  This is the main entry-point to look at, but more low-level functions are
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
  , mkAuth
  , doUpload
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

-- | Another bogus thing
dist_hup :: a -> a
dist_hup = id

