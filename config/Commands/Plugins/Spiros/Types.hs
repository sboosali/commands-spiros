{-# LANGUAGE TemplateHaskell, LambdaCase, GeneralizedNewtypeDeriving, ViewPatterns  #-}
module Commands.Plugins.Spiros.Types where
import Commands.Plugins.Spiros.Extra.Types 

import qualified Commands.Servers.Servant as Server
import qualified Commands.Backends.OSX         as OSX
import           Commands.Parsers.Earley (EarleyParser) 

-- import qualified System.FilePath.Posix as FilePath
import Control.Lens (makePrisms) 
import Data.Text.Lazy (Text) 

import Control.Monad.IO.Class (MonadIO) 


type SpirosConfig      = Server.VConfig SpirosBackend SpirosContext 

type SpirosGlobals     = Server.VGlobals SpirosContext

type SpirosBackend     = SpirosMonad -- TODO 

type SpirosMonad_      = SpirosMonad () 

type SpirosParser s r  = EarleyParser s r String Text 

newtype SpirosMonad a = SpirosMonad
 { getSpirosMonad :: OSX.WorkflowT IO a
 } deriving
 ( OSX.MonadWorkflow
 -- , MonadNatlink
 -- , MonadVServer
 -- , MonadState Server.VState
 , MonadIO

 , Monad
 , Applicative
 , Functor 
 )

-- ================================================================ --

data SpirosContext 
 = GlobalContext 
 | EmacsContext 
 | ChromeContext 
 | IntelliJContext 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)
instance NFData SpirosContext 

makePrisms ''SpirosContext 

-- ================================================================ --

