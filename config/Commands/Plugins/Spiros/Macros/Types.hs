{-# LANGUAGE OverloadedStrings, RankNTypes, LambdaCase, FlexibleContexts, GADTs, ConstraintKinds, FlexibleInstances, DataKinds        #-}
module Commands.Plugins.Spiros.Macros.Types where
import           Commands.Plugins.Spiros.Etc

import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import Data.Function (on)
import Language.Haskell.TH.Syntax (Name, mkName) 
import           GHC.Exts                        (IsString)



newtype Macro = Macro (Apply Rankable CWorkflow_)
-- type Grammatical a = (Rankable a, Show a) -- , Eq a  -- LiberalTypeSynonyms not enough 

instance Show Macro where show = show . getMacroName -- TODO lawless 
instance Eq Macro where (==) = (==) `on` getMacroName -- TODO lawless 
instance Ord Macro where compare = compare `on` getMacroName -- TODO lawless 

{-| destructor 

-}
unMacro :: Macro -> (Apply Rankable CWorkflow_)
unMacro (Macro f) = f

{-| since macros are "function-like", we give them an "identity" for equality/debugging. 

-}
getMacroName :: Macro -> Name 
getMacroName = getApplyName . unMacro

runMacro :: Macro -> CWorkflow_
runMacro = runApply . unMacro

{-| a specialized vocabulary where the macro name comes from the dict key.  

-}
vocabMacro :: (IsString t, Show t, Functor'RHS n t f) => [(String, CWorkflow_)] -> RHS n t f Macro 
vocabMacro = vocab . fmap (fmap Macro) . fmap go 
 where
 go :: (String, CWorkflow_) -> (String, Apply Rankable CWorkflow_)
 go (name,workflow) = (name, A0 (mkName name) workflow)



-- ================================================================ --

-- | "freeze" function application, up to some arity. 
-- the arguments are existentially quantified, but can be constrained.
data Apply constraint r where
 A0 :: (constraint r)                                           
    => Name ->                      r                       -> Apply constraint r    -- lol
 A1 :: (constraint a)                                           
    => Name -> (a ->                r) -> a                 -> Apply constraint r
 A2 :: (constraint a, constraint b)                             
    => Name -> (a -> b ->           r) -> a -> b            -> Apply constraint r
 A3 :: (constraint a, constraint b, constraint c)               
    => Name -> (a -> b -> c ->      r) -> a -> b -> c       -> Apply constraint r
 A4 :: (constraint a, constraint b, constraint c, constraint d) 
    => Name -> (a -> b -> c -> d -> r) -> a -> b -> c -> d  -> Apply constraint r

instance Rankable (Apply Rankable r) where rank = rankApply

-- arguments are existentially quantified 
rankApply :: Apply Rankable r -> Int
rankApply = \case
 A0 _ r         -> rank r
 A1 _ _ a       -> rank a
 A2 _ _ a b     -> safeAverage [rank a, rank b]
 A3 _ _ a b c   -> safeAverage [rank a, rank b, rank c]
 A4 _ _ a b c d -> safeAverage [rank a, rank b, rank c, rank d]

runApply :: Apply constraint r -> r
runApply = \case
 A0 _ f         -> f
 A1 _ f a       -> f a
 A2 _ f a b     -> f a b
 A3 _ f a b c   -> f a b c
 A4 _ f a b c d -> f a b c d

getApplyName :: Apply constraint r -> Name 
getApplyName = \case
 A0 name _         -> name 
 A1 name _ _       -> name 
 A2 name _ _ _     -> name 
 A3 name _ _ _ _   -> name 
 A4 name _ _ _ _ _ -> name 


