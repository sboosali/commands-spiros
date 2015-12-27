{-# LANGUAGE DeriveAnyClass, StandaloneDeriving, LambdaCase #-}
module Commands.Plugins.Spiros.Root.Types where
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Macros.Types 
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
import           Commands.Plugins.Spiros.Shortcut.Types 
import           Commands.Plugins.Spiros.Phrase.Types 

import Commands.Backends.OSX (KeyRiff) 

import Control.DeepSeq (NFData(..), deepseq) 


data Roots
 = Frozen [Stage] Root
 | Ambiguous Root
 | Macro_      Number  Macro  -- ^ repeated 
 | Root_ Root
 deriving (Show,Eq,Ord)
instance NFData Roots where     -- TODO complete the instance, we only care about Root_ for now  
 rnf = \case
  Frozen{} -> () 
  Ambiguous{} -> () 
  Macro_ n m -> n `deepseq` m `deepseq` () 
  Root_ r -> r `deepseq` () 

-- | the stages of the DSL 
data Stage = RawStage | ParseStage | RunStage deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data,NFData)

data Root
 = Acts_       [Acts]         -- ^ chained and repeated
 | Shortcut_   Number  Shortcut  -- ^ repeated 
 | Shell_              Shell  -- ^
 | Emacs_      Number  Emacs  -- ^ repeated 
 | Letters_    Letters       -- ^ 
 | Dictation_  Dictation     -- ^ 
 | Phrase_     Phrase        -- ^ 
 -- TODO | Click_ Click
 deriving (Show,Eq,Ord,Generic,Data,NFData)  -- TODO no instance for ,Generic,Data, because Macro is a GADT 

data Acts
 = ActsRW Int Act   -- ^ read/write actions
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Act
 = KeyRiff_ KeyRiff
 --TODO | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Click = Click Times Button deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

data Times = Single | Double | Triple deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData)

