

{-| 

types useful package-wide

-}

module Distribution.Hup.Types
  (
    IsCandidate(..)
  , IsDocumentation(..)
  , Package(..)
  , Upload(..)
  )
  where

import qualified Data.ByteString.Lazy as L

-- | whether a package is a normal one or a candidate
data IsCandidate = NormalPkg | CandidatePkg
  deriving (Show, Eq, Read)

-- | are we uploading a package or just docs
data IsDocumentation = IsPackage | IsDocumentation
  deriving (Show, Eq, Read)

-- | name and version of a package
data Package = Package       {  packageName    :: String
                               ,packageVersion :: String } 
               deriving (Show, Eq, Read)



-- | Bundle together information useful for an upload.
data Upload = 
    Upload { package       :: Package            -- ^ package name & version
            ,fileToUpload  :: FilePath           -- ^ file being uploaded
            ,fileConts     :: Maybe L.ByteString -- ^ file conts
            ,uploadType    :: IsDocumentation    -- ^ docco or package 
            ,isCandidate   :: IsCandidate        -- ^ candidate or not
           } deriving (Show, Eq)


