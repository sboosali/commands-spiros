{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shortcut where
import Commands.Plugins.Spiros.Etc

import Commands.Etc
import Commands.Mixins.DNS13OSX9
import Commands.Plugins.Example.Keys
import Commands.Sugar.Keys
import Commands.Backends.OSX.Types

import           Data.Text.Lazy                 (Text)

import           GHC.Exts                        (IsString (..))
-- import Control.Applicative


newtype Shortcut = Shortcut KeyRiff
 deriving (Show, Eq, Ord)

-- runShortcut :: Shortcut -> 
runShortcut (Shortcut kr) = runKeyRiff kr

shortcuts :: (Functor'RHS n Text f) => [(String,String)] -> RHS n Text f Shortcut
shortcuts
 = fmap Shortcut
 . foldMap (\(s,k) -> kbd k <$ fromString s)
 . filterBlanks

-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts = 'myShortcuts <=> shortcuts
 -- <|> "copy" $> kbd"M-c"
 -- <|> "copy" $> [KeyPress [CommandMod] CKey]
 [ "space"-: "<spc>"
 , "tab"-: "<tab>"
 , "ret"-: "<ret>"  -- "line" conflicts with (Line :: Region)
 , "del"-: "<del>"
 , "up"-: "<up>"
 , "down"-: "<down>"
 , "left"-: "<left>"
 , "right"-: "<right>"
 , "F3"-: "<f3>"
 , "F4"-: "<f4>"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 -- Global
 , "no"-: "M-z"
 , "undo"-: "M-z"
 , "salt"-: "M-a"
 , "paste"-: "M-v"
 , "copy"-: "M-c"
 , "cut"-: "M-x"
 , "history"-: "A-<spc>" -- TODO 
 , "kill"-: "C-k"

 -- Emacs
 , "stop"-: "C-g"
 , "check"-: "M-u"
 -- , "comment"-: "M-;"
 , "new file"-: "C-\\"
 , "replace"-: "M-r"
 , "mark"-: "C-<spc>"
 , "switch"-: "M-`"
 , "close buff"-: "C-x k"
 , "buffers"-: "C-x b"
 , "close"-: "C-c C-c"
 , "full-screen"-: "C-x 1"
 , "split screen"-: "C-x 2"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 -- Emacs magit-status
 , "commit"-: "c c"
 , "stage"-: "s"
 , "stage all"-: "S-s"
 , "unstage"-: "u"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 -- Chrome
 , "close tab"-: "M-w"
 , "new tab"-: "M-t"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""

 -- etc.
 , "scroll"-: "<spc>"
 , "scroll up"-: "S-<spc>"
 , "div"-: "M--"                -- (it parses) 
 , "yank"-: "C-y"               -- works in many buffer, M-v doesn't . TODO this is how we want to paste and Emacs, including phrases
 , "bookmark"-: "M-d"
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
