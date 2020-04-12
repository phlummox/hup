
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

Knowledge of the arcane and cryptic commands to be run to get
haddock docs properly built.

-}

module DocBuilding
(
-- * given a 'HupCommand', what arguments are needed
    verbosityArgs
  , haddockExtraArgs
  , cabalExtraArgs
-- * actions
  , buildDependencyDocs
  , cabalConfigure
  , cabalHaddock
  , doBuildTar
-- * top-level action
  , buildDocs
-- * path utilities
  , buildDir
)
where

import CmdArgs                    (HupCommands(..))
import Data.Text                  ( Text )
import qualified Data.Text as T
import Data.Monoid                ( (<>) )
import Shelly.Lifted
import Data.Char                  (isSpace)
import Prelude        hiding      (FilePath)
import Control.Monad.IO.Class     ( MonadIO(..) )
import qualified Stack
import Distribution.Hup           ( Package(..), buildTar )
import Control.Exception
import Shelly                     ( ReThrownException )

-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m,
--  'MonadShControl' m@
type MonadShellish m = (MonadIO m, MonadSh m, MonadShControl m)

rstrip :: Text -> Text
rstrip = T.dropWhileEnd isSpace


-- | any arguments for cabal deriving from how verbose
-- we've been asked to be
verbosityArgs :: HupCommands -> [Text]
verbosityArgs hc = if verbose hc then ["-v2"] else []

-- | arguments to supply directly to haddock, such as "--executables",
-- "--internal", or anything specified by the end user
haddockExtraArgs :: HupCommands -> [Text]
haddockExtraArgs hc =
  let args = haddockArgs hc
  in (if null args
     then []
     else ["--haddock-options=" <>T.pack(haddockArgs hc)])
   ++ (if executables hc then ["--executables"] else [])
   ++(if tests hc then ["--tests"] else [])
   ++(if internal hc then ["--internal"] else [])

-- | any arguments which need to be supplied to cabal,
-- like "--enable-tests"
cabalExtraArgs :: HupCommands -> [Text]
cabalExtraArgs hc = if tests hc then ["--enable-tests"] else []


-- | run haddock with "--only-dependencies"
buildDependencyDocs :: MonadSh m => m ()
buildDependencyDocs =
  run_ "stack" ["haddock", "--only-dependencies"]

-- | given a "base" dir, return the "dist" subdir
-- where built stuff actually gets put.
buildDir :: FilePath -> FilePath
buildDir baseDir = baseDir </> "dist"

-- | run "cabal configure" with appropriate args, prior to
--  running "cabal haddock". cabal should be on path.
--
-- args: @cabalConfigure baseDir cabalExtraArgs verbosityArgs@.
cabalConfigure
  :: MonadShellish m =>
     FilePath -> [Text] -> [Text] -> m ()
cabalConfigure baseDir cabalExtraArgs verbosityArgs =  do
  let tt = toTextIgnore
  snapshotpkgdb <- rstrip <$> silently (run "stack" ["path", "--snapshot-pkg-db"])
  localpkgdb    <- rstrip <$> silently (run "stack" ["path", "--local-pkg-db"])
  run_ "cabal" $ ["configure", "--builddir="<> tt (buildDir baseDir),
                  "--package-db=clear", "--package-db=global",
                  "--package-db=" <> snapshotpkgdb,
                  "--package-db=" <> localpkgdb] ++ verbosityArgs
                  ++ cabalExtraArgs

-- | run "cabal haddock" with appropriate args. Haddock
-- should be on path.
--
-- args: @cabalHaddock baseDir verbosityArgs haddockExtraArgs@
--
cabalHaddock
  :: MonadShellish m => FilePath -> [Text] -> [Text] -> m ()
cabalHaddock baseDir verbosityArgs haddockExtraArgs = do
  let tt = toTextIgnore
  canHyperlink <- Stack.haddockCanHyperlinkSrc
  let hyperlinkArgs = if canHyperlink
                      then ["--haddock-option=--hyperlinked-source"]
                      else []
  run_ "cabal" $ ["haddock", "--builddir=" <> tt (buildDir baseDir),
                  "--html-location=/package/$pkg-$version/docs",
                  "--contents-location=/package/$pkg-$version"]
                  ++ hyperlinkArgs ++ verbosityArgs
                  ++ haddockExtraArgs


-- | Tar up the html produced by haddock into a "-docs.tar.gz" file.
-- Returns the path of the tar file that has been built.
--
-- args: @doBuildTar baseDir package@.
doBuildTar :: MonadShellish m => FilePath -> Package -> m FilePath
doBuildTar baseDir (Package pkg_ ver_) = do
  let
    pkg = T.pack pkg_
    ver = T.pack ver_
    fromPath = T.unpack . toTextIgnore
    docTgz = fromPath $ baseDir </> (pkg<>"-"<>ver <> "-docs.tar.gz")
    docDir = pkg <> "-" <> ver <> "-docs"
  -- build tar using pure hs. or, if you have tar on system, could use:
  --    run "tar" ["cvz", "-C", dir, "--format=ustar", "-f", docTgz,
  --                pkg <> "-" <> ver <> "-docs" ]
  liftIO $ buildTar docTgz (fromPath baseDir) [T.unpack docDir]
  return $ fromText $ T.pack docTgz

-- Shelly exceptions end up being (almost) endlessly wrappered IOErrors ...
-- but here's a convenience function for catching what they look like at
-- the top level.
catch_sh_rethrown :: Sh a -> (ReThrownException SomeException -> Sh a) -> Sh a
catch_sh_rethrown = catch_sh

-- | Build a documentation .tgz file.
--
-- A Haskellified version of
-- phadej's script at
--
--    <https://github.com/phadej/binary-orphans/blob/master/hackage-docs.sh>,
--
-- which is a stack-enabled version of ekmett's script at
--
--    <https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh>.
--
-- @stackBuildDocs baseDir pkg@ will build documentation in the
-- directory @baseDir@.
--
-- Requires that stack, haddock and cabal be on the path.
--
-- Sample usage:
--
-- > :set -XOverloadedStrings
-- > let p = Package "foo" "0.1"
-- > shelly $ verbosely $ buildDocs "." p
--
-- When running from within ghci, you may have to unset some
-- environment variables that have been set. Else cabal will complain
-- about them.
--
-- > import System.Environment
-- > import Control.Monad
-- > mapM_ unsetEnv ["HASKELL_PACKAGE_SANDBOXES", "GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOX", "STACK_EXE", "HASKELL_DIST_DIR"]
--
buildDocs
  :: MonadShellish m =>
     HupCommands -> FilePath -> Package -> m FilePath
buildDocs hc tmpDir pkg =
  do
    unless (quick hc) $ do
      echo "building dependency docs"
      buildDependencyDocs
    echo "configuring"
    cabalConfigure tmpDir (verbosityArgs hc) (cabalExtraArgs hc)
    echo "running haddock"
    cabalHaddock tmpDir (verbosityArgs hc) (haddockExtraArgs hc)
    echo "copying html files"
    copyHtmlDir (toTextIgnore tmpDir) pkg
    echo "building tar file"
    doBuildTar tmpDir pkg
  where
    -- copy the html files from where haddock puts them to
    -- somewhere we can tar them from.
    copyHtmlDir :: MonadShellish m => Text -> Package -> m ()
    copyHtmlDir baseDir (Package pkg_ ver_) = do
      let pkg = T.pack pkg_
          ver = T.pack ver_
          srcDir = baseDir </> "dist" </> "doc" </> "html" </> pkg
          tgtDir = baseDir </> (pkg <> "-" <> ver <> "-docs")
      echo $ "copying from " <> toTextIgnore srcDir <> " to " <> toTextIgnore tgtDir
      liftSh $ catch_sh_rethrown (errExit False $ cp_r srcDir tgtDir) $ \e -> do
        echo $ T.unwords ["hup: Encountered exception trying to copy html"
                          , "documentation:", T.pack $ show e ]
        quietExit 1

