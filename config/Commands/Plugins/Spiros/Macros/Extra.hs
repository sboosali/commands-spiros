module Commands.Plugins.Spiros.Macros.Extra where 
import Commands.Plugins.Spiros.Macros.Types 
import Commands.Plugins.Spiros.Apply 

import Commands.Mixins.DNS13OSX9
import Commands.Backends.OSX

import GHC.Exts                        (IsString)
import Control.Arrow (second) 
import Language.Haskell.TH.Syntax (mkName)


aliasMacro :: (IsString t, Show t, Functor'RHS n t f) => (a -> CWorkflow_) -> [(String, a)] -> RHS n t f Macro 
aliasMacro f = vocabMacro . fmap (second f) 

{-| a specialized vocabulary where the macro name comes from the dict key.  

-}
vocabMacro :: (IsString t, Show t, Functor'RHS n t f) => [(String, CWorkflow_)] -> RHS n t f Macro 
vocabMacro = vocab . fmap makeMacro 

runMacro :: Macro -> CWorkflow_
runMacro = runApply . unMacro

makeMacro :: (String, CWorkflow_) -> (String, Macro)
makeMacro (name,workflow) = (name, Macro (A0 (mkName name) workflow))

