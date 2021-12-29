
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module SanityCheck where

import Control.Monad
import Control.Monad.IO.Class             (MonadIO(..))
import Control.Monad.Trans.Except         (ExceptT(..),runExceptT, throwE)

import Data.Monoid                        ( (<>) )
import Shelly                             (unlessM)
import System.Directory                   (makeAbsolute,doesFileExist )
import System.Exit

import CmdArgs                            (HupCommands(..), isBoth, isBuild, isUp)

import System.Console.CmdArgs.Implicit hiding (cmdArgs)
import qualified System.Console.CmdArgs.Implicit

cmdArgs :: Data a => a -> IO a
cmdArgs = System.Console.CmdArgs.Implicit.cmdArgs

-- TODO:
--    - does server look like a server?
--    - do we need to be careful of treating URLs and filepaths
--      as potentially bad?
--    - will ignore the sensibleness of haddockArgs, users can pass
--      what they like
--    - does the file look like a file?

-- | Run some sanity checks over a HupCommand, e.g. confirm
-- any files it refers to actually exist.
sanity :: HupCommands -> IO HupCommands
sanity !hc =  do
  putStrLn "running sanity checks"
  let !sanityTests = [fileSanity] -- TODO: add more sanity tests here <<
      -- compose them
      !composedSanityTests = foldl (>=>) return sanityTests
  !res <- runExceptT $ composedSanityTests hc
  case res of
    Left err -> do print err
                   exitFailure
    Right ok -> return ok

-- | sanity test that file exists.
fileSanity
  :: MonadIO m => HupCommands -> ExceptT String m HupCommands
fileSanity hc = case hc of
  -- ignore things without a file arg
  (isBuild -> True) -> return hc
  (isBoth -> True)  -> return hc
  (isUp -> True) -> do let f = file hc
                       absF <- liftIO $ makeAbsolute f
                       unlessM ( liftIO $ doesFileExist absF ) $
                         throwE $ "Cannot find a file '" <> f <> "'"
                       let hc' = hc { file = absF }
                       return hc'
  _               -> error "match error, shouldn't be possible"

