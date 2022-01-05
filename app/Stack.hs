
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{- |

Abstract over some of stack's functionality.

-}

module Stack
  (
    haddockCanHyperlinkSrc
  , addHaddockPath
  , cabalInstalled
  , installCabal
  , addCabalPath
  , extractPath
  )
  where

import Control.Monad.IO.Class     ( MonadIO(..) )
import Shelly.Lifted
import Data.Maybe
import Data.Text                  ( Text )
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Control.Monad
import Data.Char                  (isSpace)
--import Control.Monad.Fail         ( MonadFail )
--import Control.Monad.Trans.Maybe  (runMaybeT)


-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m,
--  'MonadShControl' m
type MonadShellish m = (MonadIO m, MonadSh m, MonadShControl m) 

-- |  @haddockCanHyperlinkSrc@ tells you whether a version of
-- haddock is on the path that can do source hyperlinking.
--  
-- (implementation: versions of haddock that can't do source hyperlinking
-- return a non-zero status code if you pass "--hyperlinked-source".)
haddockCanHyperlinkSrc :: MonadShellish m  => m Bool
haddockCanHyperlinkSrc = errExit False $ do
  silently $ run_ "haddock" ["--hyperlinked-source"]
  (==0) <$> lastExitCode

rstrip :: Text -> Text
rstrip = T.dropWhileEnd isSpace

-- | @onPath prog@ tells you whether the program
-- is on the path
onPath :: MonadSh f => FilePath -> f Bool
onPath prog =
  isJust <$> which prog

-- | check that stack is on path, or die.
stackIsOnPath :: MonadSh m => m ()
stackIsOnPath = 
  unlessM (onPath "stack") $ do
    echo "Couldn't find stack on path - do you need to install stack?"
    quietExit 1

-- | check that ghc can be executed, using stack, or die.
ghcIsOnPath :: MonadShellish m => m () 
ghcIsOnPath = do
  errExit False $ run_ "stack" ["exec", "--", "which", "ghc"]
  exitCode <- lastExitCode 
  when (exitCode /= 0) $
    terror "Something is terribly wrong - couldn't get a path for ghc. exiting"


-- | trim, convert, canonicalize a path given as Text
-- (e.g. from 'run').
extractPath :: MonadSh m => Text -> m FilePath
extractPath =  canonicalize <=< (return . fromText . rstrip) 

-- | get the directory containing ghc and haddock
ghcDir :: MonadSh m => m FilePath
ghcDir =
  extractPath =<< run "stack" ["path", "--compiler-bin"]

-- | get the snapshot binaries directory, where programs
-- are installed
snapshotBinDir :: MonadShellish m => m FilePath
snapshotBinDir = 
  (</> "bin") <$> (extractPath =<< silently (run "stack" ["path", "--snapshot-install-root"]))


-- | whether "cabal" is installed in the current snapshot.
cabalInstalled :: MonadShellish m => m Bool
cabalInstalled = do
  binDir <- snapshotBinDir 
  sub $ do
    setenv "PATH" (toTextIgnore binDir) 
    onPath "cabal"

-- | Install a @cabal@ executable, from the package @cabal-install@,
-- into the package snapshot.
installCabal :: MonadShellish m => m ()
installCabal =  
    verbosely $ run_ "stack" ["build", "--no-copy-bins", "cabal-install"]

-- | prepend haddock's directory to the path
addHaddockPath :: MonadSh m => m ()
addHaddockPath = do
  haddockDir <- extractPath =<< run "stack" ["path", "--compiler-bin"]
  prependToPath haddockDir

-- | prepend cabal's directory to the path
addCabalPath :: MonadShellish m => m ()
addCabalPath = 
  snapshotBinDir >>= prependToPath

