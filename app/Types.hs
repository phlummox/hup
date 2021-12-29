
module Types where


import Distribution.Hup.Upload


data Server =  Server        {  serverURL      :: String
                               ,serverAuth     :: Maybe Auth } 
               deriving (Show, Eq)

data GlobalOpts = GlobalOpts {  optVerbose     :: Bool
                               ,optPackageName :: Maybe String
                               ,optVersion     :: Maybe String 
                               ,optIsCandidate :: Bool }
               deriving (Show, Eq)


-- | A server url, and possibly username and/or password.
-- We may need to obtain more.
type PartialServer = (String, Maybe String, Maybe String)
 



