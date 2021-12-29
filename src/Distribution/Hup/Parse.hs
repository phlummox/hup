
{-| 

extract info from cabal files and .tgz names.

-}

module Distribution.Hup.Parse (
    module Distribution.Hup.Parse
  , module Distribution.Hup.Types 
)where

import Control.Monad.Except       (MonadError(..),when)

import Data.Char                  (isDigit, toLower, isSpace)
import Data.List                  (dropWhileEnd,isSuffixOf,stripPrefix
                                  ,intercalate)
import Data.List.Split            (splitOn)

import Data.Maybe                 (listToMaybe)
import Data.String
import System.Directory           (getDirectoryContents)
import System.FilePath            (splitExtension, splitFileName, takeExtension)

import Distribution.Hup.Types     (IsCandidate(..), IsDocumentation(..) 
                                  ,Package(..), Upload(..) )


-- | strip whitespace from end
-- 
-- >>> rstrip "abcd \t\n\r"
-- "abcd"
rstrip :: String -> String
rstrip = dropWhileEnd isSpace 

-- | strip whitespace from beginning
-- 
-- >>> lstrip "\t\n\r   abcd"
-- "abcd"
lstrip :: String -> String
lstrip = dropWhile isSpace 

-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- from NDM's <https://hackage.haskell.org/package/extra-1.5.1 extra-1.5.1>
--
-- >>> replace "el" "_" "Hello Bella" == "H_lo B_la"
-- True
-- >>> replace "el" "e" "Hello"       == "Helo"
-- True
--
-- > \xs ys -> not (null xs) ==> replace xs xs ys == ys
-- > replace "" "e" "Hello"         == undefined
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace _from _to [] = []

-- | Like 'Data.List.dropWhileEnd', but for 'Data.List.take'.
--
-- (taken from filepath-1.4.1.1)
--
-- >>> takeWhileEnd (< 10) [1, 2, 10, 3, 4, -3]
-- [3,4,-3]
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

-- | like 'Data.List.span', but from the end
--
-- >>> spanEnd (< 3) [4,3,2,1,4,3,2,1] 
-- ([4,3,2,1,4,3],[2,1])
-- >>> spanEnd (< 9) [1,2,3]
-- ([],[1,2,3])
-- >>> spanEnd (< 0) [1,2,3] 
-- ([1,2,3],[])
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p xs = (dropWhileEnd p xs, takeWhileEnd p xs)


-- | like 'Data.List.break', but from the end
--
-- >>> breakEnd (> 3) [4,3,2,1,4,3,2,1] 
-- ([4,3,2,1,4],[3,2,1])
-- >>> breakEnd (< 9) [1,2,3] 
-- ([1,2,3],[])
-- >>> breakEnd (> 9) [1,2,3] 
-- ([],[1,2,3])
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd p = spanEnd (not . p)


-- | if there's a .cabal file in the current dir, return its file name.
--
-- from NDM's <https://hackage.haskell.org/package/neil-0.10 neil-0.10> 
findCabal :: IO (Maybe Prelude.FilePath)
findCabal = do
    x <- getDirectoryContents "."
    return $ listToMaybe $ filter ((==) ".cabal" . takeExtension) x

-- | find & read contents of Cabal file from current dir, if it exists.
-- else returns empty string.
--
-- from NDM's <https://hackage.haskell.org/package/neil-0.10 neil-0.10> 
readCabal :: IO String
readCabal = do
    file <- findCabal
    case file of
        Nothing -> return []
        Just file -> readFile file


-- | @extractCabal fieldName cabalConts@:
--  extract contents of field named `fieldName` from a Cabal file string.
--
-- field name is case-insensitive [folded to lowercase]
--
-- from NDM's <https://hackage.haskell.org/package/neil-0.10 neil-0.10> 
extractCabal :: String -> String -> String
extractCabal find = f . words . replace ":" " : "
    where
        f (name:":":val:_) | map toLower find == map toLower name = val
        f (_x:xs) = f xs
        f [] = error "Failed to find the Cabal key " ++ find

-- | Inspect the name of a .tar.gz file to work out the package name 
-- and version it's for, and whether it is for documentation or a package.
parseTgzFilename
   :: (IsString s, MonadError s m) => 
       Prelude.FilePath -> m (IsDocumentation, Package)
parseTgzFilename f = do 
  let (base, ext) = splitExtension f
  ext `shouldBe` ".gz"
  (base, ext) <- return $ splitExtension base 
  ext `shouldBe` ".tar"
  base        <- return $ snd $ splitFileName base
  --let isDocco = if "-docs" `isSuffixOf` base
  --              then IsDocumentation 
  --              else IsPackage
  (base, isDocco) <- return $ if "-docs" `isSuffixOf` base
                              then let base' = intercalate "-" $ 
                                               init $ splitOn "-" base
                                   in (base', IsDocumentation)
                              else (base, IsPackage)
  let (pkg, ver) = spanVersion base
  pkg <- return $ dropWhileEnd (=='-') pkg
  return (isDocco, Package pkg ver)

  where
    ext `shouldBe` expected = 
      when (ext /= expected) $
        throwError $ fromString $ unwords ["Expected filename with extension"
                                           ,"'.tar.gz', but got", f]
    spanVersion = spanEnd (\x -> isDigit x || x == '.')

-- | 'parseTgzFilename'' specialized to 'Data.Either.Either'.
--
-- >>> (parseTgzFilename' "foo-bar-baz-0.1.0.0.2.3.0.1.tar.gz") :: Either String (IsDocumentation, Package)
-- Right (IsPackage,Package {packageName = "foo-bar-baz", packageVersion = "0.1.0.0.2.3.0.1"})
parseTgzFilename'
   :: (IsString s) => 
       Prelude.FilePath -> Either s (IsDocumentation, Package)
parseTgzFilename'  = parseTgzFilename 




