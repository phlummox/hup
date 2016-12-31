
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "lib/**/*.hs" >>= doctest

