
{- | Build tar files

-}

module Distribution.Hup.BuildTar where


import Codec.Archive.Tar as Tar (write, pack)
import qualified Codec.Compression.GZip as GZ
import Data.ByteString.Lazy as BS hiding (pack)
import Prelude hiding (read)


-- | @buildTar tarFileName baseDir paths@  -
-- create a gz-compressed tar file with name tarFileName,
-- with files in it from baseDir, "paths" being the files & directories
-- to archive, relative to baseDir.
buildTar :: FilePath -> FilePath -> [FilePath] -> IO ()
buildTar tarFileName baseDir paths = 
    BS.writeFile tarFileName . GZ.compress . write =<< pack baseDir paths 


