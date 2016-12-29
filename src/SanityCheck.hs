


module SanityCheck where

import Control.Monad
import Control.Monad.IO.Class             (MonadIO(..))
import Control.Monad.Trans.Except         (ExceptT(..),runExceptT, throwE)

import Data.Monoid                        ( (<>) )
import Shelly                             (whenM, unlessM)
import System.Directory                   (makeAbsolute,doesFileExist ) 
import System.Exit

import CmdArgs                            (HupCommands(..))
import CmdArgs.PatchHelp                  (cmdArgs)

-- TODO:
--    - does server look like a server?
--    - do we need to be careful of treating URLs and filepaths
--      as potentially bad?
--    - will ignore the sensibleness of haddockArgs, users can pass
--      what they like
--    - does the file look like a file?
sanity :: HupCommands -> IO HupCommands
sanity hc =  do
  let sanityTests = [fileSanity] -- TODO: add more sanity tests here <<
      -- compose them
      composedSanityTests = foldl (>=>) return sanityTests
  res <- runExceptT $ composedSanityTests hc
  case res of
    Left err -> do print err
                   exitFailure
    Right ok -> return ok

-- | sanity test that file exists.
fileSanity
  :: MonadIO m => HupCommands -> ExceptT String m HupCommands
fileSanity hc = case hc of
--  -- ignore things without a file arg
  Docbuild {} -> return hc
  Docboth  {} -> return hc
  _           -> do let f = file hc
                    absF <- liftIO $ makeAbsolute f
                    unlessM ( liftIO $ doesFileExist absF ) $
                      throwE $ "Cannot find a file '" <> f <> "'"
                    let hc' = hc { file = absF }
                    return hc'








              
