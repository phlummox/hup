

{- |

Support code for testing Distribution.Hup.Parse

-}
module Distribution.Hup.Parse.Test 
  (
    arbWord
  , arbUpload
  , prop_parseTgzFilename_roundtripsOK
  )
  where

import Data.List                  (intercalate)
import Data.ByteString.Lazy.Char8 (pack)
import Test.QuickCheck

import Distribution.Hup.Parse

arbWord :: Gen String
arbWord = do
  len <- choose (1, 10)
  vectorOf len $
      oneof [choose ('a', 'z')
           ,choose ('A', 'Z')]


arbName :: Gen String
arbName = do
  len <- choose (1, 4)
  intercalate "-" <$> vectorOf len arbWord

arbVersion :: Gen String
arbVersion = do
  numComponents <- choose (1,10)
  numbers <- vectorOf numComponents $ getNonNegative <$>
                                      (arbitrary :: Gen (NonNegative Int))
  return $ intercalate "." $ map show numbers

-- Generate an Upload, including an empty file contents.
arbUpload :: Gen Upload
arbUpload = do
  name   <- arbName
  ver    <- arbVersion
  isPack <- elements [IsPackage, IsDocumentation]
  isCand <- elements [NormalPkg, CandidatePkg]
  let pk   = Package name ver
      file = name ++ "-" ++ ver ++
                if isPack == IsPackage
                then ".tar.gz"
                else "-docs.tar.gz"
  return $ Upload pk file (Just $ pack "") isPack isCand

prop_parseTgzFilename_roundtripsOK :: Property
prop_parseTgzFilename_roundtripsOK  =
  forAll arbUpload $ \upl ->
    let
         parsed :: Either String (IsDocumentation, Package)
         parsed = (parseTgzFilename' $ fileToUpload upl )

    in case parsed of
            Right (isDoc, Package parsedName parsedVer) ->
                  isDoc       == uploadType upl
              &&  parsedName  == packageName    ( package upl)
              &&  parsedVer   == packageVersion ( package upl)
            Left  _msg ->
                  False



