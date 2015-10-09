{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shortcut where
import Commands.Plugins.Spiros.Shortcut.Types 
import Commands.Plugins.Spiros.Chrome.Gmail 

import Commands.Etc
import Commands.Mixins.DNS13OSX9

import Control.Applicative


-- TODO global context (e.g. all Apps) should be overridden by a local context (e.g. some App)
myShortcuts :: R z Shortcut 
myShortcuts = 'myShortcuts
 <=> globalShortcuts
 <|> emacsShortcuts
 <|> magitShortcuts
 <|> chromeShortcuts 
 <|> gmailShortcuts


globalShortcuts :: R z Shortcut  
globalShortcuts = shortcuts
 -- keys
 [ "space"-: "<spc>"
 , "tab"-: "<tab>"
 , "ret"-: "<ret>"  -- "line" conflicts with (Line :: Region)
 , "del"-: "<del>"
 , "return"-: "<ret>"
 , "delete"-: "<del>"
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

 , "no"-: "M-z"
 , "undo"-: "M-z"
 , "salt"-: "M-a"
 , "paste"-: "M-v"
 , "copying"-: "M-c"               -- 
 , "cutting"-: "M-x"                -- 
 , "kill"-: "C-k"
 , "show apps"-: "M-<tab>"
 , "twist"-: "M-<tab> <ret>"
 , "switch"-: "M-`"
 , "abdicate"-: "M-q"
 , "scroll"-: "<spc>"
 , "scroll down"-: "<spc>"
 , "scroll up"-: "S-<spc>"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]


emacsShortcuts :: R z Shortcut 
emacsShortcuts = shortcuts 
 [ "stop"-: "C-g"
 -- , "check"-: "M-u"
 -- , "comment"-: "M-;"
 , "new file"-: "C-\\"
 , "replace"-: "M-r"
 , "mark"-: "C-<spc>"
 , "close buffer"-: "C-x k"
 , "buffers"-: "C-x b"
 , "close"-: "C-c C-c"
 , "full-screen"-: "C-x 1"
 , "split screen"-: "C-x 2"
 , "other buffer"-: "C-x o"
 , "undo"-: "C-/"
 , "redo"-: "C-?"               -- NOTE undo-tree-mode
 , "evil toggle"-: "C-z"
 , "divider"-: "M--"                -- (it parses) 
 , "yank"-: "C-y"               -- works in many buffer, M-v doesn't . TODO this is how we want to paste and Emacs, including phrases
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]


magitShortcuts :: R z Shortcut 
magitShortcuts = shortcuts 
 [ "stage"-: "s"
 , "stage all"-: "S-s"
 , "unstage"-: "u"
 , "commit"-: "c c"
 , "amend"-: "c a"
 , "difference"-: "d <ret>"
 , "push"-: "S-p S-p"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]


chromeShortcuts :: R z Shortcut 
chromeShortcuts = shortcuts
 [ "new tab"-: "M-t"
 , "close tab"-: "M-w"
 , "last"-: "M-9"
 , "left tab"-: "C-S-<tab>"
 , "right tab"-: "C-<tab>"
 , "left page"-: "M-<left>"
 , "right page"-: "M-<right>"
 , "reload"-: "M-r"
 , "zoom in"-: "M-+"
 , "zoom out"-: "M--"
 , "reopen tab"-: "M-S-t"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]

