module Commands.Plugins.Spiros.Server.Types where 
import           Commands.Plugins.Spiros.Extra

import qualified Commands.Backends.OSX         as OSX

import Data.Text.Lazy (Text) 


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
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Data,Generic)

