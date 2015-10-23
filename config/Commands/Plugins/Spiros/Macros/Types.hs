{-# LANGUAGE OverloadedStrings, RankNTypes, TypeOperators, LambdaCase, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, GADTs, TypeFamilies, ConstraintKinds, DataKinds #-}

module Commands.Plugins.Spiros.Macros.Types where
import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.TypeLevel

import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import Data.Function (on)
import Language.Haskell.TH.Syntax (Name, mkName) 
import           GHC.Exts                        (IsString)


newtype Macro = Macro (Apply IsMacroArgument CWorkflow_)

instance Eq  Macro where (==)    = (==)    `on` getMacroName -- TODO lawless 
instance Ord Macro where compare = compare `on` getMacroName -- TODO lawless 

instance Show     Macro where show = showMacro -- TODO lawless 
instance Rankable Macro where rank = rankMacro

{-| an argument to a Macro must satisfy these constraints 

-}
type IsMacroArgument = '[Rankable, Show] 

showMacro :: Macro -> String
showMacro = show . getMacroName
-- showMacro

rankMacro :: Macro -> Int
rankMacro = unMacro >>> \case
 A0 _ _         -> defaultRank 
 A1 _ _ a       -> sum [rank a]
 A2 _ _ a b     -> sum [rank a, rank b]
 A3 _ _ a b c   -> sum [rank a, rank b, rank c]
 A4 _ _ a b c d -> sum [rank a, rank b, rank c, rank d]

-- | destructor 
unMacro :: Macro -> (Apply IsMacroArgument CWorkflow_)
unMacro (Macro f) = f

{-| since macros are "function-like", we give them an "identity" for equality/debugging. 

-}
getMacroName :: Macro -> Name 
getMacroName = getApplyName . unMacro

{-| a specialized vocabulary where the macro name comes from the dict key.  

-}
vocabMacro :: (IsString t, Show t, Functor'RHS n t f) => [(String, CWorkflow_)] -> RHS n t f Macro 
vocabMacro = vocab . fmap go 
 where
 go :: (String, CWorkflow_) -> (String, Macro)
 go (name,workflow) = (name, Macro (A0 (mkName name) workflow))

runMacro :: Macro -> CWorkflow_
runMacro = runApply . unMacro


-- ================================================================ --

-- | "freeze" function application, up to arity @4@.
-- the arguments are existentially quantified, but can be constrained.
data Apply constraints r where
 A0 :: ()
    => Name ->                      r                       -> Apply constraints r    -- lol
 A1 :: (EachHasEach constraints '[a])                                           
    => Name -> (a ->                r) -> a                 -> Apply constraints r
 A2 :: (EachHasEach constraints '[a,b])
    => Name -> (a -> b ->           r) -> a -> b            -> Apply constraints r
 A3 :: (EachHasEach constraints '[a,b,c])
    => Name -> (a -> b -> c ->      r) -> a -> b -> c       -> Apply constraints r
 A4 :: (EachHasEach constraints '[a,b,c,d]) 
    => Name -> (a -> b -> c -> d -> r) -> a -> b -> c -> d  -> Apply constraints r

runApply :: Apply constraints r -> r
runApply = \case
 A0 _ f         -> f
 A1 _ f a       -> f a
 A2 _ f a b     -> f a b
 A3 _ f a b c   -> f a b c
 A4 _ f a b c d -> f a b c d

getApplyName :: Apply constraints r -> Name 
getApplyName = \case
 A0 name _         -> name 
 A1 name _ _       -> name 
 A2 name _ _ _     -> name 
 A3 name _ _ _ _   -> name 
 A4 name _ _ _ _ _ -> name 

