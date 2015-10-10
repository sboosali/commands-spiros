{-# LANGUAGE ExtendedDefaultRules, FlexibleContexts  #-}
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
-- Map String Workflow
-- lookup "mark"
mark :: MonadWorkflow m=>m()
mark = press "C-<spc>"

activate_mark :: MonadWorkflow m=>m()
activate_mark = replicateM_ 2 exchange_point_and_mark

exchange_point_and_mark :: MonadWorkflow m=>m()
exchange_point_and_mark = press "C-x x"
-- exchange_point_and_mark = runEmacs "exchange-point-and-mark"

execute_extended_command :: MonadWorkflow m=>m()
execute_extended_command = press "C-w" --TODO non-standard: make this configurable? ImplicitParams? this is the configuration! just put in separate module. or define this as a keypress, and explicitly turn it into an action at  use site.

eval_expression :: MonadWorkflow m=>m()
eval_expression = press "M-:"

window_bottom :: MonadWorkflow m=>m()
window_bottom = press "M-<down>"

-- my_keymap_prefix = (kbd"M-q")
-- keymap = "M-q"                  -- NOTE nonstandard  
keymap = "C-x C-y"
