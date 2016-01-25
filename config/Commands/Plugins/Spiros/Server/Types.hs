{-# LANGUAGE DeriveAnyClass, TypeOperators, RankNTypes  #-}
module Commands.Plugins.Spiros.Server.Types where 
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Types (SpirosContext) 
import           Commands.Plugins.Spiros.Root.Types(Roots)   

import           Commands.Mixins.DNS13OSX9 as Dragon
import qualified Commands.Backends.OSX         as OSX
import           Commands.Servers.Servant


import Data.Text.Lazy (Text) 


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

type SpirosInterpreterSettings = InterpreterSettings SpirosMonad SpirosType

type SpirosMonad    = OSX.Workflow 


-- ================================================================ --

data InterpreterSettings m a = InterpreterSettings 
 { iExecute :: m :~>: IO 
 , iRanking :: Ranking a 
 , iMagic   :: ServerMagic a
 } 

type ServerMagic a = CommandsHandlers a OSX.Workflow_ -> AmbiguousParser a -> Ranking a -> [Text] -> a -> IO Bool -- TODO

type AmbiguousParser a = [Text] -> (Maybe a, [a])

type CommandsRequest = [Text]

data CommandsResponse a b = CommandsResponse 
 { rRaw       :: [Text]
 , rParsed    :: Maybe a 
 , rDesugared :: Maybe b 
 }

data CommandsHandlers a b = CommandsHandlers
 { hParse   :: [Text] -> Maybe a 
 , hDesugar :: a -> b 
 }

data Mode
 = ModeNormal 
 | ModeCorrecting 
 | ModeDictating 
 | ModeSleeping 
 | ModeOff 
 | ModeReading
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

