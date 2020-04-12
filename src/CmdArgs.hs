
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module CmdArgs where

import Data.Version            (showVersion)
import Paths_hup               (version)
import System.Console.CmdArgs hiding(cmdArgs)
import System.Environment      (getArgs, withArgs)

import qualified DefaultServerUrl
import CmdArgs.PatchHelp (cmdArgs)

isDoc :: HupCommands -> Bool
isDoc cmd = case cmd of
  Docbuild {} -> True
  Docboth {} -> True
  _           -> False

isBuild :: HupCommands -> Bool
isBuild cmd = case cmd of
  Docbuild {}   -> True
  Packbuild {}  -> True
  _             -> False

isBoth :: HupCommands -> Bool
isBoth cmd = case cmd of
  Docboth {}  -> True
  Packboth {} -> True
  _           -> False

isUp :: HupCommands -> Bool
isUp cmd = case cmd of
  Docup {} -> True
  Packup {} -> True
  _         -> False




defaultServer :: String
defaultServer = DefaultServerUrl.defaultServerUrl

-- | Actions the program can perform
data HupCommands =
    Docbuild  { verbose  :: Bool
                , executables :: Bool
                , tests :: Bool
                , internal :: Bool
                ,haddockArgs :: String
                ,quick :: Bool }
  | Docboth   { verbose  :: Bool
              , executables :: Bool
              , tests :: Bool
              , internal :: Bool
              , haddockArgs :: String
              , quick :: Bool
              , server   :: String
              , candidate :: Bool
              , user     :: Maybe String
              , password :: Maybe String  }

  | Packbuild { verbose :: Bool }

  | Packup    { verbose  :: Bool
              , server   :: String
              , candidate :: Bool
              , user     :: Maybe String
              , password :: Maybe String
              , file     :: String }

  | Packboth  { verbose  :: Bool
              , server   :: String
              , candidate :: Bool
              , user     :: Maybe String
              , password :: Maybe String
              }

  | Docup     { verbose  :: Bool
              , server   :: String
              , candidate :: Bool
              , user     :: Maybe String
              , password :: Maybe String
              , file     :: String }
      deriving (Show, Eq, Data, Typeable, Ord)

isUpload :: HupCommands -> Maybe HupCommands
isUpload Docbuild {} = Nothing
isUpload x           = Just x

-- Helpers for specifying metavariables etc. for
-- arguments.

{-# INLINE serverArg #-}
serverArg :: Data val => val -> val
serverArg x = x &= typ  "URL"

{-# INLINE userArg #-}
userArg :: Data val => val -> val
userArg   x = x &= typ  "USER"

{-# INLINE passwdArg #-}
passwdArg :: Data val => val -> val
passwdArg x = x &= typ  "PASSWORD"

{-# INLINE fileArg #-}
fileArg :: Data val => val -> val
fileArg x = x &= typ "FILE"

{-# INLINE verbArgs #-}
verbArgs :: Data val => val -> val
verbArgs x = x &= help "be verbose"

-- commands that can be run

packbuild :: HupCommands
packbuild =
  Packbuild
    { verbose     = verbArgs  def
    }
      &= help     "Build source distribution .tgz for a package."


packup :: HupCommands
packup =
  Packup
    { verbose    = verbArgs   def
      ,server    = serverArg  defaultServer
      ,candidate =            def
      ,file      = fileArg    def  &= argPos 0
      ,user      = userArg    Nothing
      ,password  = passwdArg  Nothing }
       &= help     (unwords   ["Upload FILE as a package (or"
                               ,"candidate package)."])

packboth :: HupCommands
packboth =
  Packboth
    { verbose    = verbArgs   def
      ,server    = serverArg  defaultServer
      ,candidate =            def
      ,user      = userArg    Nothing
      ,password  = passwdArg  Nothing }
       &= help     (unwords   ["Build source distribution .tgz and upload"
                               ,"as package (or candidate package)."])

docbuild :: HupCommands
docbuild =
  Docbuild
    { verbose     = verbArgs  def
     ,executables =           def &= help "Run haddock for Executables targets"
     ,tests       =           def &= help "Run haddock for Test Suite targets"
     ,internal    =           def &= help (unwords ["Run haddock for internal"
                                           ,"modules and include all symbols"])
     ,haddockArgs =           def &= help "extra args to pass to haddock"
                                  &= explicit
                                  &= name "haddock-arguments"
                                  &= typ  "ARGS"
     ,quick       =           def &= help (unwords ["quick build - don't build"
                                          ,"docco for dependencies (links may"
                                          ,"be broken)"])
     }
      &= help     "Build documentation for a package."

docup :: HupCommands
docup =
  Docup
    { server = serverArg  defaultServer
     ,file   = fileArg    def &= argPos 0
     }
     &= help "Upload FILE as documentation."

docboth :: HupCommands
docboth =
  Docboth
    {}
    &= help "Build and upload documentation for a package."

-- Process command-line arguments
processArgs :: IO HupCommands
processArgs = do
   args <- getArgs
    -- If the user did not specify any arguments, pretend "--help" was given
   (if null args then withArgs ["--help"] else id) proc

  where
  proc :: IO HupCommands
  proc = cmdArgs $ -- commands that can be run, i.e. "modes"
           modes [packbuild
                 ,packup
                 ,packboth
                 ,docbuild -- &= groupname "A"
                 ,docup    -- &= groupname "B"
                 ,docboth ]  -- &= groupname "C"]
                  &= help progHelp
                  &= program "hup"
                  &= summary ("hup v" ++ showVersion version)
                  &= helpArg [explicit, name "h", name "help"]

  progHelp = unwords
     ["Build and/or upload packages or documentation to a hackage server."
     ,"A server url should be of the format PROTOCOL://SERVER[:PORT]/,"
     ,"and defaults to", defaultServer, "if not specified.\n"
     ,"\nA password can also be given in the PASSWORD environment"
     ,"variable instead of on the command line.\n", "\n'hup --help=all'"
     ,"will give help for all commands." ]

