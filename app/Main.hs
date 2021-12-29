
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main where

import Prelude hiding (FilePath)

import Control.Monad
import Control.Monad.IO.Class     ( MonadIO(..) )
import Control.Monad.Reader       (MonadReader(..), runReaderT)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..),runExceptT,throwE)
import Data.Either                (either)
import Data.Text                  ( Text )
import qualified Data.Text as T
import Data.Monoid                ( (<>) )
import Shelly.Lifted
import System.IO                  (hSetBuffering, BufferMode( LineBuffering )
                                  , stdout)
--import qualified System.IO.Temp as Tmp

import Distribution.Hup           ( Package(..),IsDocumentation(..)
                                  , IsCandidate(..),Auth(..),Upload(..)
                                  , mkAuth
                                  , readCabal, extractCabal
                                  , parseTgzFilename'
                                  , getUploadUrl)
import CmdArgs                    (HupCommands(..), isUpload, processArgs
                                  , isBuild, isBoth, isDoc)
import DocBuilding                (buildDocs)
import SanityCheck                (sanity)
import qualified Stack
import Upload                     (doUpload)





-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m,
-- 'MonadShControl' m, 'MonadReader' 'HupCommands' m@
type MonadHup m = (MonadIO m, MonadSh m, MonadShControl m, MonadReader HupCommands m)

-- | Build documentation tgz, return an Upload value,
-- return an Upload value, ready to be uploaded. (Or not, as desired.)
--
-- This installs cabal-install if it is not found in the snapshot
-- binaries, and adds the path to the cabal executable, and to
-- haddock, to the path.
--
-- Sample usage:
--
-- > import Distribution.Hup.Upload
-- > :set -XOverloadedStrings
-- > let p = Package "foo" "0.1"
-- > let d = Docbuild { verbose = True }
-- > upload <- shelly $ verbosely $ runReaderT (stackBuildDocs "." p) d
-- > doUpload "http://localhost:8080" upload (mkAuth "myname" "mypass")
--
-- When running from within ghci, you may have to unset some
-- environment variables that have been set.
--
-- > import System.Environment
-- > import Control.Monad
-- > mapM_ unsetEnv ["HASKELL_PACKAGE_SANDBOXES", "GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOX", "STACK_EXE", "HASKELL_DIST_DIR"]
stackBuildDocs :: MonadHup m => FilePath -> Package -> m Upload
stackBuildDocs tmpDir pkg = do
  hc <- ask
  -- check paths for the tools we need
  Stack.addHaddockPath
  echo "checking for cabal"
  haveCabal <- Stack.cabalInstalled
  unless haveCabal $ do
    echo "didn't find 'cabal' binary in snapshot directory - installing it"
    Stack.installCabal
  Stack.addCabalPath
  -- build the tgz file
  docTgz <- toTextIgnore <$> buildDocs hc tmpDir pkg
  -- return an Upload value, ready to be uploaded. (Or not, as desired.)
  return $ Upload pkg (T.unpack docTgz) Nothing IsDocumentation (isCand hc)


-- | run "stack sdist" and copy tarball to current dir.
--
-- sample use - something like:
--
-- > let p = Package "foo" "0.1"
-- > let d = Pacbuild { verbose = True }
-- > shelly $ verbosely $ runReaderT (stackSourceDist p) d
--
stackSourceDist :: MonadHup m => Package -> m Upload
stackSourceDist p@(Package pkg ver) = do
  -- work out flags to call with ...
  hc <- ask
  run_ "stack" ["sdist"] -- if there are errors, just let them
                         -- be re-thrown
  distDir <- Stack.extractPath =<< run "stack" ["path", "--dist-dir"]
  let tgzFile = pkg <> "-" <> ver <> ".tar.gz"
  cp (distDir </> tgzFile) "."
  return $ Upload p tgzFile Nothing IsPackage (isCand hc)


-- | if we have a username, then we need to get
-- a password, either from the command-line or the env
getAuth :: MonadSh m => HupCommands -> m (Maybe Auth)
getAuth hc = runMaybeT $ do
  hc <- MaybeT $ return $ isUpload hc
  u <- MaybeT $ return $ user hc
  case password hc of
    Just p -> MaybeT $ return $ mkAuth u p
    Nothing -> do x <- get_env "PASSWORD"
                  case x of
                    Nothing -> terror "username specified, but no password"
                    Just p  -> MaybeT $ return $ mkAuth u (T.unpack p)


-- Use for "early return"
data Done = Done
  deriving (Show)

type MonadDone m a = ExceptT Done m a

runEarlyReturn :: Monad m => MonadDone m () -> m ()
runEarlyReturn f =
  either (const ()) id <$> runExceptT f

-- exit early
done :: Monad m => ExceptT Done m a
done = throwE Done

isCand :: HupCommands -> IsCandidate
isCand hc =
  if candidate hc
  then CandidatePkg
  else NormalPkg

-- | Look at a FILE command-line arg of something we've been asked to
-- upload, & try uploading it.
--
-- Will throw exceptions if the file doesn't exist, or doesn't look
-- like a .tar.gz file, or if we've been asked to upload docco &
-- it looks like a source file.
--
-- If the upload fails due to a bad status, however, it should
-- give a hopefully comprehensible message then end early.
--
-- todo: give nice error messages, rather than throwing exceptions
-- in some cases?
uploadTgz ::
    (MonadSh m, MonadIO m, MonadReader HupCommands m) =>
    IsDocumentation -> Text -> MonadDone m ()
uploadTgz expectedType desc = do
  hc <- ask
  let fileName   = file hc
      fileName'' = T.pack fileName
      candType   = isCand hc
      serverUrl  = server hc
      verb       = verbose hc
  (upType, Package pkg ver) <- let parsed = parseTgzFilename' fileName
                               in either (lift . terror) return parsed
  when (upType /= expectedType) $
    lift $ terror $ T.unwords ["Expected", desc, "file, got", fileName'']
  -- if all is ok, do the upload.
  let upload = Upload (Package pkg ver) (file hc) Nothing expectedType candType
  auth <- lift $ getAuth hc
  let url = getUploadUrl serverUrl upload
  lift $ echo $ "uploading to " <> T.pack url
  serverResponse <- liftIO $ doUpload serverUrl upload auth
  let displayedMesg msg = "Uploaded successfully" <>
                            (if verb
                            then T.pack msg
                            else "")
  case serverResponse of
    Left err -> do lift $ echo $ "Error from server:\n" <> T.pack err
                   throwE Done
    Right msg  -> lift $ echo $ displayedMesg msg



-- | Run a hup command (which contains details of server url to use,
-- user authentication details, etc.)
--
-- sample usage:
--
-- > let d = Docbuild { verbose = True }
-- > shelly $ verbosely $ runReaderT $ mainSh d
mainSh :: MonadHup m => m ()
mainSh =  do
  hc <- ask
  --tmpBase <- liftIO $ Tmp.getCanonicalTemporaryDirectory
  --tmpDir  <- fromText . T.pack <$> liftIO (Tmp.createTempDirectory tmpBase "hup")
  --do

  -- *  Bug intermittently occurred where this temp directory disappeared
  --    partway thru mainSh running. Might've been caused by something
  --    not being strict enough? This seems to fix it, so far *shrug*
  withTmpDir $ \tmpDir -> do
    !_ <- runEarlyReturn $ do
      cabalConts <- liftIO readCabal
      let packageName = extractCabal "name" cabalConts
          packageVer  = extractCabal "version" cabalConts
      case hc of
        Packup {}   -> do uploadTgz IsPackage "package"
                          throwE Done
        Docup  {}   -> do uploadTgz IsDocumentation "documentation"
                          throwE Done
        (isBuild -> True) -> return () -- i.e. carry on.
        (isBoth  -> True) -> return ()
        _                 -> error "match error"
      -- if still here, we've been asked to do a build.
      uploadable <- do let p = Package packageName packageVer
                       buildRes <- case hc of
                          (isDoc -> True) ->  lift $ stackBuildDocs tmpDir p
                          _               ->  lift $ stackSourceDist p

                       let tgzFile = fromText $ T.pack $ fileToUpload buildRes

                       case hc of
                         Packbuild {} -> throwE Done -- build only
                         Docbuild {} -> lift (cp tgzFile ".") >>
                                        throwE Done -- no need to upload,
                                                    -- only build requested
                         _           -> return buildRes
      auth     <- lift $ getAuth hc
      let url = getUploadUrl (server hc) uploadable
      lift $ echo $ "uploading to " <> T.pack url
      response <- liftIO $ doUpload (server hc) uploadable auth
      case response of
        Left err -> do lift $ echo $ "Error from server:\n'" <> T.pack err
                       throwE Done
        Right msg -> lift $ do echo "Uploaded successfully"
                               echo $ "mesg was: " <> T.pack msg
    return ()



main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hupCommand <- sanity =<< processArgs
  let verbosify = if verbose hupCommand
                  then verbosely
                  else id
  shelly $
    verbosify $
      runReaderT mainSh hupCommand
  return ()



