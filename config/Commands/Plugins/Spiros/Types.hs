{-# LANGUAGE TemplateHaskell, LambdaCase, GeneralizedNewtypeDeriving, ViewPatterns  #-}
module Commands.Plugins.Spiros.Types where
import Commands.Plugins.Spiros.Extra.Types 
import Commands.Plugins.Spiros.Root.Types (Roots) 

import           Commands.Mixins.DNS13OSX9 as Dragon
import qualified Commands.Backends.OSX         as OSX
import           Commands.Servers.Servant (V,VHandler,VSettings,VEnvironment,VPlugin,VConfig,VGlobals, DNSResponse) 

-- import qualified System.FilePath.Posix as FilePath
import Control.Lens 

import Control.Monad.IO.Class (MonadIO) 


type SpirosResponse    = SpirosV DNSResponse

type SpirosV           = V SpirosBackend SpirosContext SpirosType 

type SpirosHandler   i = VHandler SpirosBackend SpirosContext SpirosType i 

type SpirosSettings    = VSettings SpirosBackend SpirosContext SpirosType  

type SpirosEnvironment = VEnvironment SpirosBackend SpirosContext SpirosType  

type SpirosPlugin      = VPlugin SpirosBackend SpirosContext SpirosType 

type SpirosConfig      = VConfig SpirosBackend SpirosContext 

type SpirosGlobals     = VGlobals SpirosContext

type SpirosCommand     = Dragon.DNSEarleyCommand SpirosContext SpirosType 

type SpirosBackend     = OSX.CWorkflow 

-- type SpirosContext     = 

type SpirosType        = Roots

type SpirosMonad_    = OSX.Workflow 

newtype SpirosMonad a = SpirosMonad
 { getSpirosMonad :: OSX.WorkflowT IO a
 } deriving
 ( OSX.MonadWorkflow
 -- , MonadNatlink
 -- , MonadVServer
 -- , MonadState VState
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

-- ================================================================ --

makePrisms ''SpirosContext 
