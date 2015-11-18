{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, ViewPatterns  #-}
module Commands.Plugins.Spiros.Types where
import Commands.Plugins.Spiros.Extra.Types 

import qualified System.FilePath.Posix as FilePath

import           Data.Data    (Data) 
import           GHC.Generics (Generic)


-- | an ordinal number. i.e. "first", "second", "third", "fourth", et cetera  
newtype Ordinal = Ordinal { unOrdinal :: Integer }  -- NOTE a GeneralizedNewtypeDeriving Show, doesn't show field accessories in constructor
 deriving (Show,Read,Eq,Ord,Enum,Num,Integral,Real,Generic,Data,NFData)

data SpirosContext 
 = GlobalContext 
 | EmacsContext 
 | ChromeContext 
 | IntelliJContext 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData SpirosContext 

readSpirosContext :: String -> SpirosContext 
readSpirosContext = \case 
 (isEmacs -> Just{}) -> EmacsContext 
 "Google Chrome" -> ChromeContext 
 "IntelliJ" -> IntelliJContext 
 _ -> GlobalContext 

isEmacs :: FilePath -> Maybe FilePath
isEmacs fp = if FilePath.takeBaseName fp `elem` ["Emacs","Work","Notes","Diary","Obs","Commands"]
 then Just fp
 else Nothing

