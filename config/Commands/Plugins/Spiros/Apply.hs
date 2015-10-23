{-# LANGUAGE TypeOperators, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds, DataKinds #-}
module Commands.Plugins.Spiros.Apply where 
import Commands.Plugins.Spiros.TypeLevel
import Commands.Plugins.Spiros.Extra

import Language.Haskell.TH.Syntax (Name, mkName) 


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

instance Functor (Apply constraints) where fmap = mapApply

mapApply :: (a -> b) -> (Apply constraints a -> Apply constraints b)
mapApply g = \case
 A0 n x         -> A0 n (g x)
 A1 n f a       -> A1 n (g.f)     a
 A2 n f a b     -> A2 n (g.!!f)   a b
 A3 n f a b c   -> A3 n (g.!!!f)  a b c
 A4 n f a b c d -> A4 n (g.!!!!f) a b c d

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

