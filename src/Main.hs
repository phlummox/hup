
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Prelude hiding (FilePath)
import qualified Prelude

import Control.Monad --- ---XX
import Control.Monad.IO.Class     ( MonadIO(..) )
import Control.Monad.Reader --- ---XX
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..),runExceptT,throwE)
import Data.Char                  (isSpace, isDigit)
import Data.Either                (either)
import Data.IORef
import Data.List                  (dropWhileEnd)
import qualified Data.List as L
import Data.Maybe                 (isNothing)
import Data.Text                  ( Text(..) )
import qualified Data.Text as T
import Data.Monoid                ( (<>) )
import Shelly.Lifted
import System.Directory -- ---XXXX
import System.IO                  (hSetBuffering, BufferMode( LineBuffering )
                                  , stdout)
import System.IO.Unsafe           (unsafePerformIO)

import Distribution.Hup           (
                                    Package(..),IsDocumentation(..)
                                  ,IsCandidate(..),Auth(..),Upload(..)
                                  ,doUpload,mkAuth
--import Distribution.Hup.BuildTar  
                                  ,buildTar
--import Distribution.Hup.Parse  
                                  ,findCabal, readCabal, extractCabal
                                  ,parseTgzFilename)
import Types                      (Server(..))
import CmdArgs                    (HupCommands(..), isUpload, processArgs)
import CmdArgs.PatchHelp          (tmpXX)

import qualified Distribution.Hup as DH 

dist_hupX = DH.dist_hup
--buildTar2 = DH.buildTar

-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m, 'MonadShControl' m
type MonadShellish m = (MonadIO m, MonadSh m, MonadShControl m) 

-- | just a convenience alias, short for @'MonadIO' m, 'MonadSh' m, 'MonadShControl' m, 'MonadReader' 'HupCommands' m@
type MonadHup m = (MonadIO m, MonadSh m, MonadShControl m, MonadReader HupCommands m)

rstrip :: Text -> Text
rstrip = T.dropWhileEnd isSpace 

-- | does haddock have ability to do hyperlinked source.
haddockCanHyperlinkSrc :: MonadShellish m  => m Bool
haddockCanHyperlinkSrc = errExit False $ do
  run_ "haddock" ["--hyperlinked-source"]
  (==0) <$> lastExitCode

-- | safe version of "maximum"
maxMay xs = if null xs then Nothing else Just $ maximum xs

addGhcPath :: MonadShellish m => m ()
addGhcPath = 
  -- see if ghc is already in path;
  -- if not, try looking for it in stack's "programs" directory.
  whenM (isNothing `liftM` which "ghc") $ do
    progPath <- rstrip <$> (silently $ run "stack" ["path", "--programs"])
    let myFilt x = "ghc" `L.isPrefixOf` x && isDigit (last x)
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
-- import System.Environment
-- import Control.Monad
-- mapM_ unsetEnv ["HASKELL_PACKAGE_SANDBOXES", "GHC_PACKAGE_PATH", "HASKELL_PACKAGE_SANDBOX", "STACK_EXE", "HASKELL_DIST_DIR"]
--
--
-- TODO: ensure any exceptions handled gracefully
stackBuildDocs
  :: MonadHup m => FilePath -> Package -> m Upload
stackBuildDocs dir (Package pkg ver) = do
  hc <- ask
  let isCand =   if candidate hc
                  then CandidatePkg
                  else NormalPkg 

  canHyperlink <- haddockCanHyperlinkSrc 
  let builddir= toTextIgnore $ dir </> "dist"
      hyperlinkFlag = if canHyperlink
                      then ["--haddock-option=--hyperlinked-source"]
                      else []
  pkg <- return $ T.pack pkg
  ver <- return $ T.pack ver
  echo "build dependencies docs"
  run_ "stack" ["haddock", "--only-dependencies"]
  snapshotpkgdb <- rstrip <$> silently (run "stack" ["path", "--snapshot-pkg-db"])
  localpkgdb    <- rstrip <$> silently (run "stack" ["path", "--local-pkg-db"])
  let verboseCommands = if (verbose hc) then ["-v2"] else [] ---XXXMONREADER
  let haddockExtraArgs = let args = haddockArgs hc 
                         in if null args
                            then []
                            else ["--haddock-options=" 
                                  <> T.pack(haddockArgs hc)]
  let cabalExtraArgs =(if executables hc then ["--executables"] else [])
                    ++(if tests hc then ["--tests"] else [])
                    ++(if internal hc then ["--internal"] else [])
        
--                    ++(if tests hc then ["--tests"] else [])

  echo "configuring"
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
  return $ Upload (Package (up pkg) (up ver)) docTgz IsDocumentation isCand




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

done :: Monad m => ExceptT Done m a
done = throwE Done

fromString = fromText . T.pack


-- can prob split this out.
uploadTgz :: 
    (MonadSh m, MonadIO m, MonadReader HupCommands m) =>
    String -> IsDocumentation -> Text -> MonadDone m ()
uploadTgz serverUrl expectedType desc = do 
  hc <- ask
  let fileName = file hc 
      candType = if candidate hc
                then CandidatePkg
                else NormalPkg 

  auth <- lift $ getAuth hc


  fileExists <- lift $ test_e $ fromString fileName
  when (not fileExists) $ 
    lift $ terror $ T.pack $ unwords ["file", fileName, "doesn't exist"]
  fileReg <- lift $ test_f $ fromString fileName
  when (not fileReg) $ 
    lift $ terror $ T.pack $ unwords ["file", fileName, "isn't a readable file"]


  (upType, Package pkg ver) <- getTgzDetails2
  when (upType /= expectedType) $
    lift $ terror $ T.unwords ["Expected", desc, "file, got",
                               T.pack $ file hc]
  let upload = Upload (Package pkg ver) (file hc)  expectedType candType 
  -- lift $ inspect upload
  serverResponse <- liftIO $ doUpload serverUrl upload auth
  case serverResponse of 
    Left err -> do lift $ echo $ "Error from server:\n" <> T.pack err
                   throwE Done 
    Right _  -> return ()

getTgzDetails2 :: (MonadSh m, MonadIO m, MonadReader HupCommands m) => 
                  ExceptT Done m (IsDocumentation, Package)
getTgzDetails2= ask >>= \hc ->
                  case parseTgzFilename $ file hc of
                      Left err -> lift $ terror err
                      Right x  -> return x



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
    let v = verbose hc
        candType = if candidate hc
                  then CandidatePkg
                  else NormalPkg 
        -- we drop any trailing slashes, "doUpload" will add them
        -- as needed.
        serverUrl = dropWhileEnd (=='/') $ server hc
    auth <- lift $ getAuth hc
    cabalConts <- liftIO readCabal
    let packageName = extractCabal "name" cabalConts
        packageVer  = extractCabal "version" cabalConts 

    -- if just doing upload ...
    case hc of
      -- factor out common code
      -- handle authentication
      Packup {}   -> do uploadTgz serverUrl   
                                       IsPackage "package"  
                        throwE Done

      Docup  {}   -> do uploadTgz serverUrl  IsDocumentation 
                                       "documentation"  
                        throwE Done
      _           -> return ()

    uploadable <- do let p = Package packageName packageVer
                     buildRes <- lift $ stackBuildDocs tmpDir p  
                     let tgzFile = fromText $ T.pack $ fileToUpload buildRes 
                     case hc of 
                       Docbuild {} -> lift (cp tgzFile ".") >>
                                      throwE Done
                       _           -> return buildRes
    response <- liftIO $ doUpload serverUrl uploadable auth
    case response of 
      Left err -> do lift $ echo $ "Error from server:\n'" <> T.pack err
                     throwE Done 
      Right _  -> return ()



checkPrereqs :: MonadSh m => m ()
checkPrereqs = do
  
  whenM (isNothing <$> which "stack") $  
    echo "Couldn't find stack on path - do you need to install stack?"

  let apps = ["cabal", "haddock"]

  forM_ apps $ \app -> 
    whenM (isNothing <$> which (fromText app)) $ do
      echo $ T.unwords ["Couldn't find", app, "on path - do you need to"
                        ,"run 'install", app <> "'? Exiting"]
      quietExit 1

main = do
  hSetBuffering stdout LineBuffering
  hupCommand <- processArgs
  let verbosify = if verbose hupCommand
                  then verbosely
                  else id
  shelly $ do
    silently $ checkPrereqs
    verbosify $ do 
      addGhcPath
      runReaderT mainSh hupCommand 
  return ()





