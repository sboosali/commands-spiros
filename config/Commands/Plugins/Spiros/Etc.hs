{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances, ViewPatterns, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Etc where

-- import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX
import           Commands.Sugar.Press

import qualified System.FilePath.Posix as FilePath

import Data.Foldable
import qualified Data.List as List


{- | returns the digits that make up a number in some base, most-significant digit first. 

>>> toDigits 10 9
[9]

>>> toDigits 10 1234
[1,2,3,4]

assumes nonnegative input, but still is total. 

prop> \(Positive k) n -> ('fromDigits' k . toDigits k) n == n

-}
toDigits :: (Integral a) => a -> a -> [a]
toDigits base = reverse . List.unfoldr go
 where
 go n = if n < 1 then Nothing else Just ((n `mod` base), (n `div` base))

{- | returns the digits that make up a number in some base, most-significant digit first. 

>>> fromDigits 10 [1,2,3,4]
1234

>>> fromDigits 10 [9]
9

>>> fromDigits 10 []
0

-}
fromDigits :: forall a. (Integral a) => a -> [a] -> a
fromDigits base = foldr (+) 0 . zipWith (\i n -> (base^i) * n) [(0::a)..] . reverse

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

type Desugaring a = a -> Actions_

type Ranking a = a -> Int

-- lawless, domain-specific, 'Ord'-like typeclass.
-- used by @data Apply@, permitting "ranking a function" by ranking its arguments (before application). 
-- can be derived, avoiding boilerplate. 
class Rankable a where          -- TODO a ranking that's relative, not absolute.
 rank :: Ranking a
 rank _ = defaultRank

defaultRank :: Int
defaultRank = 100

highRank :: Int
highRank = 1000

instance Rankable Int where rank = id
instance Rankable Actions_         -- TODO
instance (Rankable a, Rankable b) => Rankable (Either a b) where rank = either rank rank

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

-- used when sendText is too slow/laggy
-- insertByClipboard :: String -> AMonadAction_
insertByClipboard :: String -> Actions_
insertByClipboard s = restoringClipboard $ do
 setClipboard s
 press_paste

press_paste :: Actions_
press_paste = press M 'v'

-- runs the action, then restores the previous clipboard contents. dictation still pollutes clipboard history, but the most recent "manual" clipboard contents should be preserved.  
-- benign race condition, as no lock is kept on the system clipboard
restoringClipboard :: Actions a -> Actions a
restoringClipboard m = do
 contents <- getClipboard
 x <- m
 delay 100 -- otherwise, e.g. the old clipboard contents are reset before the temporary clipboard contents are paste TODO call Haskell from Objective-C on callback? 
 setClipboard contents
 return x

