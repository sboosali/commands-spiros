{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances, ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Etc where

-- import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import qualified System.FilePath.Posix as FilePath

import Data.Foldable
import qualified Data.List as List


type Number = Int

-- average xs = realToFrac (sum xs) / genericLength xs
safeAverage :: Foldable t => t Int -> Int
safeAverage (toList -> []) = 0
safeAverage xs = sum xs `div` length xs

nothing :: (Monad m) => m ()
nothing = return ()

onlyWhen :: (Monad m) => (b -> Maybe b) -> b -> (m () -> m ())
onlyWhen predicate_ question_ action_ = maybe nothing (const action_) (predicate_ question_)

slot :: String -> AMonadAction_
slot s = do
 delay 10
 sendText s
 sendKeyPress [] ReturnKey

isDefaultBrowser :: AMonadAction (Maybe String)
isDefaultBrowser = currentApplication >>= \case
 x@"Google Chrome" -> return$ Just x
 _                 -> return$ Nothing 

-- lawless, domain-specific, 'Ord'-like typeclass
class    Rankable a where
 rank :: a -> Int
 rank _ = defaultRank

defaultRank :: Int
defaultRank = 100

highRank :: Int
highRank = 1000

instance Rankable Int where rank = id

instance Rankable Actions_         -- TODO

isBrowser x = if FilePath.takeBaseName x `elem` ["Firefox", "Chrome"]
 then Just x
 else Nothing

defaultDelay = 100 :: Int

browserDelay = 250 :: Int                 -- milliseconds
 -- in chrome, keypresses are lost when the delay isnt long enough 

runRepeat :: (MonadAction m) => Int -> Number -> (m () -> m ())
runRepeat delay_ times_
 = traverse_ id
 . List.intersperse (delay delay_)
 . replicate times_
 --TODO action grouping: insert nullop between each, for logging

