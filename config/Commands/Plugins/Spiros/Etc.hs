{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances, ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Etc where

-- import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import Data.Foldable
-- import Data.List


-- average xs = realToFrac (sum xs) / genericLength xs
safeAverage :: Foldable t => t Int -> Int
safeAverage (toList -> []) = 0
safeAverage xs = sum xs `div` length xs

nothing :: (Monad m) => m ()
nothing = return ()

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

instance Rankable Int where rank = id

instance Rankable Actions_         -- TODO
