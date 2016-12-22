
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Prelude hiding (FilePath)
import qualified Prelude

import Control.Monad.IO.Class     ( MonadIO(..) )
import Control.Monad.Reader       (MonadReader(..), runReaderT, liftM)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..),runExceptT,throwE)
import Data.Char                  (isSpace, isDigit)
import Data.Either                (either)
import Data.IORef
import Data.List                  (dropWhileEnd,isPrefixOf)
import Data.Maybe                 (isNothing)
import Data.Text                  ( Text(..) )
import qualified Data.Text as T
import Data.Monoid                ( (<>) )
import Shelly.Lifted
import System.Directory           (listDirectory)
import System.IO                  (hSetBuffering, BufferMode( LineBuffering )
                                  , stdout)

import Distribution.Hup           ( Package(..),IsDocumentation(..)
                                  ,IsCandidate(..),Auth(..),Upload(..)
                                  ,mkAuth ,buildTar
                                  ,findCabal, readCabal, extractCabal
                                  ,parseTgzFilename, parseTgzFilename' 
                                  , getUploadUrl)
import Types                      (Server(..))
import CmdArgs                    (HupCommands(..), isUpload, processArgs)
import Upload                     (doUpload)


-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m,
--  'MonadShControl' m
type MonadShellish m = (MonadIO m, MonadSh m, MonadShControl m) 

-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m,
-- 'MonadShControl' m, 'MonadReader' 'HupCommands' m@
type MonadHup m = (MonadIO m, MonadSh m, MonadShControl m, MonadReader HupCommands m)

rstrip :: Text -> Text
rstrip = T.dropWhileEnd isSpace 

-- | does haddock have ability to do hyperlinked source.
haddockCanHyperlinkSrc :: MonadShellish m  => m Bool
haddockCanHyperlinkSrc = errExit False $ do
  run_ "haddock" ["--hyperlinked-source"]
  (==0) `liftM` lastExitCode

-- | safe version of "maximum"
maxMay xs = if null xs then Nothing else Just $ maximum xs

-- | look for ghc & try to add it to path
addGhcPath :: MonadShellish m => m ()
addGhcPath = 
  -- see if ghc is already in path;
  -- if not, try looking for it in stack's "programs" directory.
  whenM (isNothing `liftM` which "ghc") $ do
    progPath <- rstrip <$> silently (run "stack" ["path", "--programs"])
    let myFilt x = "ghc" `isPrefixOf` x && isDigit (last x)
    ghcDir <- liftIO $ (maxMay . filter myFilt) `liftM` listDirectory (T.unpack progPath)
    case ghcDir of
      Nothing -> terror "couldn't find ghc on path, please add it"
      Just ghcDir' -> appendToPath $ progPath </> ghcDir' </> "bin"


-- | Run commands for building documentation tgz. A Haskellified version of
-- phadej's script at
--    https://github.com/phadej/binary-orphans/blob/master/hackage-docs.sh,
-- which is a stack-enabled version of ekmett's script at
--    https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh.
--
-- `stackBuildDocs dir pkg CandidatePkg` will build documentation in the
-- directory `dir`, and will return an `Upload` object, which can be uploaded
-- with `Distribution.Hup.Upload.doUpload`.
--
-- Requires that stack, haddock and cabal be on the path.
--
-- Sample usage:
--
-- > import Distribution.Hup.Upload 
-- > :set -XOverloadedStrings 
-- > let p = Package "foo" "0.1"
-- > upload <- shelly $ stackBuildDocs "." p CandidatePkg 
-- > doUpload "http://localhost:8080" upload (mkAuth "myname" "mypass") 
--
-- When running from within ghci, you may have to unset some
-- environment variables that have been set.
--
-- > import System.Environment
-- > import Control.Monad
-- > mapM_ unsetEnv ["HASKELL_PACKAGE_SANDBOXES", "GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOX", "STACK_EXE", "HASKELL_DIST_DIR"]
--
stackBuildDocs :: MonadHup m => FilePath -> Package -> m Upload
stackBuildDocs dir (Package pkg ver) = do
  hc <- ask
  -- work out flags to call with ...
  canHyperlink <- haddockCanHyperlinkSrc 
  let hyperlinkFlag = if canHyperlink
                      then ["--haddock-option=--hyperlinked-source"]
                      else []
  unless (quick hc) $ do
    echo "build dependencies docs"
    run_ "stack" ["haddock", "--only-dependencies"]
  snapshotpkgdb <- rstrip <$> silently (run "stack" ["path", "--snapshot-pkg-db"])
  localpkgdb    <- rstrip <$> silently (run "stack" ["path", "--local-pkg-db"])
  let verboseCommands  = if verbose hc then ["-v2"] else [] 
  let haddockExtraArgs = let args = haddockArgs hc 
                         in if null args
                            then []
                            else ["--haddock-options=" <>T.pack(haddockArgs hc)]
  let cabalExtraArgs =(if executables hc then ["--executables"] else [])
                    ++(if tests hc then ["--tests"] else [])
                    ++(if internal hc then ["--internal"] else [])
  echo "configuring"
  let builddir= toTextIgnore $ dir </> "dist"
  run "cabal" $["configure", "--builddir="<>builddir, 
               "--package-db=clear", "--package-db=global", 
               "--package-db=" <> snapshotpkgdb, 
               "--package-db=" <> localpkgdb] ++ verboseCommands
  echo "making docs"
  run "cabal" $["haddock", "--builddir=" <> builddir,  
               "--html-location=/package/$pkg-$version/docs",
               "--contents-location=/package/$pkg-$version"] 
               ++ hyperlinkFlag ++ verboseCommands
               ++ haddockExtraArgs
  pkg <- return $ T.pack pkg
  ver <- return $ T.pack ver
  let srcDir = builddir </> "doc" </> "html" </> pkg
      tgtDir = dir </> (pkg <> "-" <> ver <> "-docs")
  cp_r srcDir tgtDir
  echo "tarring"
  let
    up = T.unpack
    fromPath = T.unpack . toTextIgnore 
    docTgz = fromPath $ dir </> (pkg<>"-"<>ver <> "-docs.tar.gz") 
    docDir = pkg <> "-" <> ver <> "-docs" 
  -- or, if you have tar on system, could use:
  --    run "tar" ["cvz", "-C", dir, "--format=ustar", "-f", docTgz,
  --                pkg <> "-" <> ver <> "-docs" ]
  liftIO $ buildTar docTgz (fromPath dir) [up docDir]
  return $ Upload (Package (up pkg) (up ver)) docTgz IsDocumentation (isCand hc)


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

isCand hc = 
  if candidate hc
  then CandidatePkg
  else NormalPkg 


fromString = fromText . T.pack

-- | check that stack, cabal & haddock are on path
checkPrereqs :: MonadSh m => m ()
checkPrereqs = do
  whenM (isNothing <$> which "stack") $  do
    echo "Couldn't find stack on path - do you need to install stack?"
    quietExit 1
  whenM (isNothing <$> which "cabal") $ do
    echo $ T.unwords ["Couldn't find cabal on path - do you need to"
                      ,"run 'stack install cabal-install'? Exiting"]
    quietExit 1
  whenM (isNothing <$> which "haddock") $ do
    echo $ T.unwords ["Couldn't find haddock on path - do you need to"
                      ,"run 'stack install haddock'? Exiting"]
    quietExit 1


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
    String -> IsDocumentation -> Text -> MonadDone m ()
uploadTgz serverUrl expectedType desc = do 
  hc <- ask
  let fileName = file hc 
      fileName' = fromString fileName
      fileName'' = T.pack fileName
      candType = isCand hc 
  -- check file actually exists & is regular (not directory)
  whenM (lift $ not `liftM` test_e fileName') $ 
    lift $ terror $ T.pack $ unwords ["file", fileName, "doesn't exist"]
  fileReg <- lift $ test_f $ fromString fileName
  when (not fileReg) $ 
    lift $ terror $ T.pack $ unwords ["file", fileName, "isn't a readable file"]

  (upType, Package pkg ver) <- let parsed = parseTgzFilename' fileName
                               in either (lift . terror) return parsed 
  when (upType /= expectedType) $
    lift $ terror $ T.unwords ["Expected", desc, "file, got", fileName'']
  -- if all is ok, do the upload.
  let upload = Upload (Package pkg ver) (file hc)  expectedType candType 
  auth <- lift $ getAuth hc
  let url = getUploadUrl serverUrl upload
  lift $ echo $ "uploading to " <> T.pack url
  serverResponse <- liftIO $ doUpload serverUrl upload auth
  case serverResponse of 
    Left err -> do lift $ echo $ "Error from server:\n" <> T.pack err
                   throwE Done 
    Right _  -> lift $ echo "Uploaded successfully"



-- | Run a hup command (which contains details of server url to use,
-- user authentication details, etc.)
--
-- sample usage:
--
-- > let d = Docbuild { verbose == True }
-- > shelly $ verbosely $ runReaderT $ mainSh d
mainSh :: MonadHup m => m ()
mainSh =  do  
  hc <- ask
  withTmpDir $ \tmpDir -> 
    runEarlyReturn $ do
      cabalConts <- liftIO readCabal
      let packageName = extractCabal "name" cabalConts
          packageVer  = extractCabal "version" cabalConts 
      case hc of
        Packup {}   -> do uploadTgz (server hc) IsPackage "package"  
                          throwE Done
        Docup  {}   -> do uploadTgz (server hc) IsDocumentation "documentation"
                          throwE Done
        _           -> return () -- i.e. carry on.
      -- if still here, we've been asked to do a build.
      uploadable <- do let p = Package packageName packageVer
                       buildRes <- lift $ stackBuildDocs tmpDir p  
                       let tgzFile = fromText $ T.pack $ fileToUpload buildRes 
                       case hc of 
                         Docbuild {} -> lift (cp tgzFile ".") >>
                                        throwE Done
                         _           -> return buildRes
      auth     <- lift $ getAuth hc
      let url = getUploadUrl (server hc) uploadable
      lift $ echo $ "uploading to " <> T.pack url
      response <- liftIO $ doUpload (server hc) uploadable auth
      case response of 
        Left err -> do lift $ echo $ "Error from server:\n'" <> T.pack err
                       throwE Done 
        _        -> return ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hupCommand <- processArgs
  let verbosify = if verbose hupCommand
                  then verbosely
                  else id
  shelly $ do
    silently checkPrereqs
    verbosify $ do 
      addGhcPath
      runReaderT mainSh hupCommand 
  return ()



