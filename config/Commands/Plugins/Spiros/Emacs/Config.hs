{-# LANGUAGE ExtendedDefaultRules #-}
module Commands.Plugins.Spiros.Emacs.Config where

import           Commands.Sugar.Alias
import           Commands.Sugar.Press
import           Commands.Backends.OSX

import qualified System.FilePath.Posix as FilePath

import Control.Monad

-- default ((), Actions_)


isEmacs :: FilePath -> Maybe FilePath
isEmacs fp = if FilePath.takeBaseName fp `elem` ["Emacs","Work","Notes","Diary","Obs","Commands"]
 then Just fp
 else Nothing

-- TODO read application from environment, which determines the keyboard shortcut
-- an application is defined by the keyboard shortcuts it supports?
-- Rec?
-- Map String Actions
-- lookup "mark"
mark :: Actions_
mark = press C spc

activate_mark :: Actions_
activate_mark = replicateM_ 2 exchange_point_and_mark

exchange_point_and_mark :: Actions_
exchange_point_and_mark = press C 'x' >> press C 'x'
-- exchange_point_and_mark = runEmacs "exchange-point-and-mark"

execute_extended_command :: Actions_
execute_extended_command = press C 'w' --TODO non-standard: make this configurable? ImplicitParams? this is the configuration! just put in separate module. or define this as a keypress, and explicitly turn it into an action at  use site.

eval_expression :: Actions_
eval_expression = press M ':'
