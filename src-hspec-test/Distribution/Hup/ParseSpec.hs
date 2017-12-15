

module Distribution.Hup.ParseSpec where

import Test.Hspec
import qualified Distribution.Hup.Parse.Test

arbWord =  Distribution.Hup.Parse.Test.arbWord 

prop_parseTgzFilename_roundtripsOK =
  Distribution.Hup.Parse.Test.prop_parseTgzFilename_roundtripsOK


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "parseTgzFilename" $ 
    it "should round-trip back to original name, version, package type"  
      prop_parseTgzFilename_roundtripsOK


