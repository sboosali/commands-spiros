{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, RankNTypes  #-}
module Commands.Plugins.Spiros.Server.Types where 
import           Commands.Plugins.Spiros.Module (SpirosType, SpirosMonad, SpirosMonad_) 
import           Commands.Plugins.Spiros.Extra

import Data.Text.Lazy (Text) 


type SpirosInterpreterSettings = InterpreterSettings SpirosMonad SpirosType -- TODO 

data InterpreterSettings m a = InterpreterSettings 
 { iExecute :: m :~>: IO 
 , iRanking :: Ranking a 
 , iMagic   :: ServerMagic a
 } 

type ServerMagic a = CommandsHandlers a SpirosMonad_ -> AmbiguousParser a -> Ranking a -> [Text] -> a -> IO Bool -- TODO

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
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data)
instance NFData Mode 
