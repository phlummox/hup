

module Distribution.Hup.ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Distribution.Hup.Parse

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "parseTgzFilename" $ 
    it "should round-trip back to original name, version, package type" $ 
      property prop_parseTgzFilename_roundtripsOK


