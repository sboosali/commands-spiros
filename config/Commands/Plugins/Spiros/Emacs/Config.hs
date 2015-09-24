{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Spiros.Emacs.Config where
-- import           Commands.Plugins.Spiros.Keys

import           Commands.Sugar.Keys
import           Commands.Backends.OSX

import qualified System.FilePath.Posix as FilePath

import Control.Monad

-- default ((), Actions_)


emacsDelay = 10 :: Int                 -- milliseconds

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
mark = press_ "C-<spc>"

activate_mark :: Actions_
activate_mark = replicateM_ 2 exchange_point_and_mark

exchange_point_and_mark :: Actions_
exchange_point_and_mark = press_ "C-x x"
-- exchange_point_and_mark = runEmacs "exchange-point-and-mark"

execute_extended_command :: Actions_
execute_extended_command = press_ "C-w" --TODO non-standard: make this configurable? ImplicitParams? this is the configuration! just put in separate module. or define this as a keypress, and explicitly turn it into an action at  use site.

eval_expression :: Actions_
eval_expression = press_ "M-:"

window_bottom :: Actions_
window_bottom = press_ "M-<down>"

-- my_keymap_prefix = (kbd"M-q")
keymap = "M-q"                  -- NOTE nonstandard  

