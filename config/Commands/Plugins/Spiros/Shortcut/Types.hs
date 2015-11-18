{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Shortcut.Types where
import Commands.Plugins.Spiros.Extra.Types 

import Commands.Extra
import Commands.Mixins.DNS13OSX9
import Commands.Backends.OSX

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))


newtype Shortcut = Shortcut KeyRiff
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData)

runShortcut (Shortcut kr) = runKeyRiff kr

shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f Shortcut
shortcuts
 = fmap Shortcut
 . foldMap (\(s,k) -> kbd k <$ fromString s)
 . filterBlanks

