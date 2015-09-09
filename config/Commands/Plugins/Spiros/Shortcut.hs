{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shortcut where

import Commands.Etc
import Commands.Mixins.DNS13OSX9
import Commands.Plugins.Example.Keys

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))
import Control.Applicative


shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f KeyRiff
shortcuts = foldMap $ \case
 ("","") -> empty               -- for convenience
 (s, k) -> keys k <$ fromString s

-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts = 'myShortcuts <=> shortcuts
 -- <|> "copy" $> keys"M-c"
 -- <|> "copy" $> [KeyPress [CommandMod] CKey]
 [ "space"-: "<spc>"
 , "tab"-: "<tab>"
 , "line"-: "<ret>"
 , "del"-: "<del>"
 , "up"-: "<up>"
 , "down"-: "<down>"
 , "left"-: "<left>"
 , "right"-: "<right>"
 , "F3"-: "<f3>"
 , "F4"-: "<f4>"
 -- Global
 , "undo"-: "M-z"
 , "salt"-: "M-a"
 , "paste"-: "M-v"
 , "copy"-: "M-c"
 , "cut"-: "M-x"
-- , "history"-: "A-" -- TODO 
 , "kill"-: "C-k"

 -- Emacs
 , "stop"-: "C-g"
 , "check"-: "M-u"
 , "comment"-: "M-;"
 , "new file"-: "C-\\"
 , "replace"-: "M-r"
 , "switch"-: "M-~"
 , "mark"-: "C-<spc>"
 , "switch"-: "M-`"
 , "close buff"-: "C-x k" 
 , "close"-: "C-c C-c"

 -- Chrome
 , "close tab"-: "M-w"
 , "new tab"-: "M-t"

 -- etc.
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]
