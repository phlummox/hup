

{-| 

types useful package-wide

-}

module Distribution.Hup.Types
where

-- | whether a package is a normal one or a candidate
data IsCandidate = NormalPkg | CandidatePkg
  deriving (Show, Eq)

-- | are we uploading a package or just docs
data IsDocumentation = IsPackage | IsDocumentation
  deriving (Show, Eq)

-- | name and version of a package
data Package = Package       {  packageName    :: String
                               ,packageVersion :: String } 
               deriving (Show, Eq)




