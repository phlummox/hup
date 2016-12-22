
{-# LANGUAGE CPP #-}

{- |

Bits of cmdargs-0.10.14.1, included purely to add some extra
info to the output of "--help".

-}

module CmdArgs.PatchHelp where


#ifdef PATCH_HELP

import Data.Char (toLower, isDigit)
import System.Console.CmdArgs.Annotate
import System.Console.CmdArgs.Explicit hiding (flagHelpFormat)
import System.Console.CmdArgs.Implicit.Internal.Global hiding(global,extraFlags)
import System.Console.CmdArgs.Implicit.Internal.Local
import System.Console.CmdArgs.Implicit hiding(cmdArgs,cmdArgsMode,cmdArgsCapture)
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Implicit.Internal.Reform(reform)

tmpXX = 'a'


{-
-- | Create a help flag triggered by @-?@/@--help@. The user
--   may optionally modify help by specifying the format, such as:
--
-- > --help=all          - help for all modes
-- > --help=html         - help in HTML format
-- > --help=100          - wrap the text at 100 characters
-- > --help=100,one      - full text wrapped at 100 characters
--
-- From System.Console.CmdArgs.Explicit.flagHelpFormat
-}
flagHelpFormat :: (HelpFormat -> TextFormat -> a -> a) -> Flag a
flagHelpFormat f = (flagOpt "" ["help","?"] upd "" "Display help message. '--help=all' will display help for all commnds. '--help=bash' will output code for bash command-line completion."){flagInfo = FlagOptRare ""}
    where
        upd s v = case format s of
            Left e -> Left e
            Right (a,b) -> Right $ f a b v

        format :: String -> Either String (HelpFormat,TextFormat)
        format xs = foldl (\acc x -> either Left (f x) acc) (Right def) (sep xs)
            where
                sep = words . map (\x -> if x `elem` ":," then ' ' else toLower x)
                f x (a,b) = case x of
                    "all" -> Right (HelpFormatAll,b)
                    "one" -> Right (HelpFormatOne,b)
                    "def" -> Right (HelpFormatDefault,b)
                    "html" -> Right (a,HTML)
                    "text" -> Right (a,defaultWrap)
                    "bash" -> Right (HelpFormatBash,Wrap 1000000)
                    "zsh"  -> Right (HelpFormatZsh ,Wrap 1000000)
                    _ | all isDigit x -> Right (a,Wrap $ read x)
                    _ -> Left "unrecognised help format, expected one of: all one def html text <NUMBER>"

global :: Prog_ -> Mode (CmdArgs Any)
global x = setReform (reform y) $ setHelp y $ setProgOpts x $ collapse $ assignGroups y
    where y = assignNames $ extraFlags x


extraFlags :: Prog_ -> Prog_
extraFlags p = p{progModes = map f $ progModes p}
    where f m = m{modeFlags_ = modeFlags_ m ++ flags}
          grp = if length (progModes p) > 1 then Just commonGroup else Nothing
          wrap x = def{flagFlag=x, flagExplicit=True, flagGroup=grp}
          flags = changeBuiltin_ (progHelpArg p) (wrap $ flagHelpFormat $ error "flagHelpFormat undefined") ++
                  changeBuiltin_ (progVersionArg p) (wrap $ flagVersion vers) ++
                  [wrap $ flagNumericVersion $ \x -> x{cmdArgsVersion = Just $ unlines v}
                        | Just v <- [progNumericVersionOutput p]] ++
                  changeBuiltin_ (fst $ progVerbosityArgs p) (wrap loud) ++
                  changeBuiltin_ (snd $ progVerbosityArgs p) (wrap quiet)
          [loud,quiet] = flagsVerbosity verb
          vers x = x{cmdArgsVersion = Just $ unlines $ progVersionOutput p}
          verb v x = x{cmdArgsVerbosity = Just v}


cmdArgsCapture :: Data a => Capture Ann -> Mode (CmdArgs a)
cmdArgsCapture = remap embed proj . global . local
    where embed = fmap fromAny
          proj x = (fmap Any x, embed)

-- | Take impurely annotated records and turn them in to a 'Mode' value, that can
--   make use of the "System.Console.CmdArgs.Explicit" functions (i.e. 'process').
--
--   Annotated records are impure, and will only contain annotations on
--   their first use. The result of this function is pure, and can be reused.
cmdArgsMode :: Data a => a -> Mode (CmdArgs a)
cmdArgsMode = cmdArgsCapture . capture

-- | Take impurely annotated records and run the corresponding command line.
--   Shortcut for @'cmdArgsRun' . 'cmdArgsMode'@.
--
--   To use 'cmdArgs' with custom command line arguments see
--   'System.Environment.withArgs'.
cmdArgs :: Data a => a -> IO a
cmdArgs = cmdArgsRun . cmdArgsMode

#else

import System.Console.CmdArgs.Implicit hiding (cmdArgs)
import qualified System.Console.CmdArgs.Implicit 

cmdArgs :: Data a => a -> IO a
cmdArgs = System.Console.CmdArgs.Implicit.cmdArgs 

#endif

