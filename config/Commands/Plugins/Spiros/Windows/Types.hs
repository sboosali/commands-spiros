{-# LANGUAGE DeriveFunctor #-}
-- | (you can read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Windows.Types where
import Commands.Plugins.Spiros.Extra (Generic, Data) 


{-| 

-}
data BatchScriptR t = BatchScriptR 
 { __hostDirectory__    :: t 
 , __guestDirectory__   :: t 
 , __natlinkDirectory__ :: t 
 , __natlinkFile__      :: t 
 -- , :: t 
 } deriving (Show,Eq,Ord,Functor,Data,Generic)

