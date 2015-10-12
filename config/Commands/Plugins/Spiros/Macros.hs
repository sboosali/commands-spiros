{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts, GADTs, ConstraintKinds, FlexibleInstances, DataKinds            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Macros where
import Commands.Plugins.Spiros.Types
import Commands.Plugins.Spiros.Macros.Types
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Edit
import Commands.Plugins.Spiros.Emacs.Config
import           Commands.Plugins.Spiros.Shell
import Commands.Plugins.Spiros.Keys
import  Commands.Plugins.Spiros.Template

import Commands.Sugar.Keys
import           Commands.Etc
import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import           Control.Applicative
import qualified Data.List as List
import Control.Arrow (second) 
import Control.Monad (replicateM_) 

-- default (Workflow ())            -- ExtendedDefaultRules 


myMacros :: R z Macro
myMacros = 'myMacros <=> empty 
 <|> myMacrosN
 <|> myMacros0

alt_tab = do
 press "M-<tab>"
 press "<ret>" 

move_window_down = press "S-<down>"

move_window_up = press "S-<up>"

toggle_alfred            = press "M-<spc>"               -- NOTE Alfred
toggle_clipboard_history = press "A-<spc>"               -- NOTE Alfred

open_pad = do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_down
   delay 100
   switch_buffer "*pad*" -- TODO make variable 
   delay 100

emacs_reach_shell = do
   move_window_down 
   delay 25
   switch_buffer "*shell*"
   delay 25
   window_bottom                -- TODO make typed/generic like runEdit
   runMove (MoveTo Beginning Line)
   runEdit (Edit Delete Forwards Line)

emacs_reach_repl = do
   move_window_down 
   delay 25
   switch_buffer "*commands-spiros*" -- TODO
   delay 25
   window_bottom                -- TODO make typed/generic like runEdit
   runMove (MoveTo Beginning Line)
   runEdit (Edit Delete Forwards Line)

youtube_toggle_sound = do
   app <- currentApplication
   openApplication "Google Chrome" -- TODO make variable 
   chrome_reach_youtube
   press "M-<up>"
   delay chromeDelay 
   youtube_toggle_fullscreen 
   delay chromeDelay
   press "k"      -- pauses the video, somehow  
   delay chromeDelay 
   youtube_toggle_fullscreen 
   delay 2000
   openApplication app

chrome_reach_youtube = do
 switch_tab "YouTube.com"

youtube_toggle_fullscreen = press "S-f"

global_reach_voice_app = do 
 openApplication "Commands"
 delay 100



-- ================================================================ --

-- | all macros without arguments (should be a VocabularyList)
myMacros0 :: R z Macro 
myMacros0 = 'myMacros0
 <=> myMacros0_
 <|> myAliases
 <|> myApps
 <|> myOrdinals

-- | macros without arguments
myMacros0_ :: R z Macro 
myMacros0_ =  vocabMacro
 [ "voice"-: do                   -- short for "commands server"
   openApplication "Terminal"   -- TODO make less stringly-typed
   press "<del>" 

 , "voice build"-: do   -- for bootstrapping 
   global_reach_voice_app
   emacs_reach_shell
   slot "cabal build server"

 , "voice run"-: do   -- for bootstrapping 
   global_reach_voice_app
   emacs_reach_shell
   slot "cabal run server"

 , "voice rebel"-: do           -- REPL
   global_reach_voice_app
   emacs_reach_repl 
   slot ":r Commands.Plugins.Spiros" 

 , "voice notes"-: do
   global_reach_voice_app
   switch_buffer "notes"

 , "run again"-: do
   execute_extended_command
   press "<up>"
   press "<ret>"

 , "eval again"-: do
   eval_expression
   press "<up>"
   press "<ret>"

 , "to do"-: do
   insert "TODO "               -- TODO instance IsString Phrase' would overlap with instance IsString [a] 

 , "next error"-: do
   move_window_down
   runEmacs "compilation-next-error"
   press "C-l"
   press "C-l"
   press "<ret>" 

 , "macro"-: do   -- TODO LOL 
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_up
   delay 100
   switch_buffer "Macro.hs" -- TODO make variable 
   delay 100

   press "M-<up>"
   press "C-s" >> slot " , \"\"-: do" >> replicateM_ 6 (press "<left>") 
   -- press "C-s" >> slot " , \"\"-: do" >> press "<left><right>"
   -- press "C-s" >> slot "\""           >> press "<left><right>"

 , "shortcut"-: do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_up
   delay 100
   switch_buffer "Shortcut.hs" -- TODO make variable 
   delay 100

 -- TODO make "C-x C-y" the commands key prefix
 , "copy register"-: do
   press "C-x C-y r c" -- nonstandard: my-register 

 , "paste register"-: do
   press "C-x C-y r v" -- nonstandard: my-register 

 , "clear register"-: do
   press "C-x C-y r d" -- nonstandard: my-register 

 , "magic"-: do
   runEmacs "magit-status"

 , "music"-: do
   openApplication "Google Chrome" 
   chrome_reach_youtube

 , "pause"-: do
   youtube_toggle_sound

 , "play"-: do
   youtube_toggle_sound

 , "fun"-: do
   alt_tab

 , "check"-: do
   runEmacs "compile"
   -- press keymap >> press "c"

 , "exec again"-: do
   press "S-<down>"
   press "M-<down>"
   press "C-<up>"
   press "<ret>"

 , "phonetic alphabet"-: do
   insertByClipboard$ (List.intercalate "\n" . map fst) phoneticAlphabet 

 , "replace again"-: do
   press "M-r"
   press "<ret>"
   press "!"

 , "transfer"-: do
   press "M-c"
   openApplication "Notes"
   press "M-<down>"
   delay 500                    -- must wait for clipboard 
   press "M-v"
   replicateM_ 2 $ press "<ret>"
   delay 500                    -- must wait for clipboard 
   alt_tab

 , "haddock"-: do
   insertTemplate haddockTemplate 

 , "occur again"-: do
   multi_occur "" 
   replicateM_ 2 $ press "<up>" 
   press "<ret>"

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 , ""-: do
   nothing

 ]

-- | macros without arguments
myAliases :: R z Macro             -- TODO String
myAliases = vocabMacro$ fmap (second sendText) -- TODO embed into any phrase. in grammar itself? or, with less accuracy, just in phrase runner 
 [ ""-: ""
 , "arrow"-: "->"
 , "to do"-: "TODO"
 , "I owe unit"-: "IO ()"
 , "ret"-: "\n"
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
 , ""-: ""
 , ""-: ""
 ]

myApps :: R z Macro
myApps = vocabMacro $ fmap (second openApplication)  -- TODO make less stringly-typed
 [ ""      -: ""
 , "man"      -: "Commands"
 , "work"     -: "Work"
 , "notes"    -: "Notes"
 , "jobs"     -: "Obs"
 , "chrome"   -: "Google Chrome"
 , "fox"      -: "Firefox"
 , "terminal" -: "Terminal"
 , "dragon"   -: "VirtualBoxVM"
 , "jelly"    -: "IntelliJ"
 , ""         -: ""
 , ""         -: ""
 , ""         -: ""
 , ""         -: ""
 ]

myOrdinals :: R z Macro 
myOrdinals = (vocabMacro . fmap (fmap runOrdinalKeyChord)) dictOrdinalDigit
 -- __inlineRHS__ because: we want myMacrosRHS0 to be flattened into a vocabulary
 -- the cast is safe because: ordinalDigit is between zero and nine, inclusive

-- | run an ordinal as a keypress.
-- @runOrdinalKeyChord (Ordinal 3)@ is like @press "M-3"@. 
runOrdinalKeyChord :: Ordinal -> CWorkflow_
runOrdinalKeyChord
 = uncurry sendKeyChord
 . ordinal2keypress

ordinal2keypress :: Ordinal -> KeyChord
ordinal2keypress 
 = addMod CommandModifier
 . (either __BUG__ id)
 . digit2keypress
 . unOrdinal



-- ================================================================ --

-- | macros with arguments
myMacrosN :: R z Macro 
myMacrosN = fmap Macro $ empty

 <|>  A1  'align_regexp             align_regexp               <$           "align"     <*>  phrase
 <|>  A1  'switch_buffer            switch_buffer              <$           "buffer"    <*>  phrase
 <|>  A1  'multi_occur              multi_occur                <$           "occur"     <*>  phrase
 <|>  A1  'google_for               google_for                 <$           "google"    <*>  (phrase-?-"")
 <|>  A1  'search_regexp            search_regexp              <$           "search"    <*>  (phrase-?)
 <|>  A1  'find_text                find_text                  <$           "discover"  <*>  (phrase-?-"")
 <|>  A1  'goto_line                goto_line                  <$           "go"        <*>  number
 <|>  A1  'comment_with             comment_with               <$           "comment"   <*>  (phrase-?)
 <|>  A1  'write_to_pad             write_to_pad               <$           "scribble"  <*>  (phrase-?)
 <|>  A1  'run_shell                run_shell                  <$           "shell"     <*>  (shell-|-(phrase-?))
 <|>  A1  'query_clipboard_history  query_clipboard_history    <$           "clipboard" <*>  ((ordinalDigit-|-phrase)-?)
 <|>  A1  'query_alfred             query_alfred               <$           "Alfred"    <*>  (phrase-?)
 <|>  A1  'switch_tab               switch_tab                 <$           "tab"       <*>  (phrase-?-"")
 <|>  A1  'visit_site               visit_site                 <$           "visit"     <*>  (phrase-?-"")
 <|>  A1  'chrome_click_link        chrome_click_link          <$           "link"      <*>  phrase
 <|>  A1  'open_application         open_application           <$           "open"      <*>  dictation  
 <|>  A1  'bookmark_it              bookmark_it                <$           "bookmark"  <*>  (dictation-?)
 <|>  A1  'slotP                    slotP                      <$           "slot"      <*>  phrase 
 
 <|>  A2  'replace_with             replace_with               <$           "replace"   <*>  phrase <*"with" <*> phrase

-- TODO this elisp expression aligns the block of code, when {{M-x eval-last-sexp}}
-- "<\\$" "<\\*>"

-- we need the Apply constructors to delay function application, which allows the parser to disambiguate by ranking the arguments, still unapplied until execution

align_regexp p = do
 runEmacs "align-regexp"
 insertP p

-- needs (setq confirm-nonexistent-file-or-buffer 'after-completion), which only switches to a buffer without prompt when that buffer already exists
switch_buffer p = do
 press "C-x b"
 slotP p

multi_occur p = do
 runEmacs "multi-occur-in-matching-buffers"
 slot "."                       -- match all buffers 
 insertP p                     -- match this regexp 

replace_with this that = do
 runEmacsWithP "replace-regexp" [this, that]

google_for p = do
 q <- munge p
 google q

search_regexp p = do
  press "C-s"
  maybe nothing insertP p

find_text p = do
 press "M-f"
 delay browserDelay
 insertP p

goto_line :: Int -> CWorkflow_
goto_line n = do
 press "M-g"    -- TODO generalize to AMonadAction_, as well as PressFun https://github.com/AJFarmar/haskell-polyvariadic
 -- press (n::Int) 
 slot (show n)

comment_with :: Maybe Phrase -> CWorkflow_
comment_with p = do
 press "M-;"
 maybe nothing insertP p

write_to_pad p = do
 open_pad
 replicateM_ 2 $ press "<ret>" 
 maybe nothing insertP p

run_shell (Left s) = do
 emacs_reach_shell
 runShell s
run_shell (Right p) = do
 emacs_reach_shell
 maybe nothing insertP p

query_clipboard_history :: Maybe (Either Ordinal Phrase) -> CWorkflow_
query_clipboard_history Nothing = do
 toggle_clipboard_history
query_clipboard_history (Just (Left n)) = do 
 toggle_clipboard_history
 delay 500
 runOrdinalKeyChord n
query_clipboard_history (Just (Right p)) = do
 toggle_clipboard_history
 delay 500
 insertP p

query_alfred p = do
 toggle_alfred 
 delay 500
 maybe nothing insertP p

switch_tab p = do
 press "A-t"                    -- needs Tab Ahead chrome extension 
 delay chromeDelay 
 insertP p
 delay chromeDelay            -- NOTE {{slotP p}} doesn't work because chrome inserts text too slowly 
 press "<ret>" 

visit_site p = do
 openApplication "Google Chrome"   -- TODO make variable 
 press "M-t"
 delay chromeDelay 
 slotP p

-- http://superuser.com/questions/170353/chrome-selecting-a-link-by-doing-search-on-its-text
chrome_click_link p = do
 press "M-f"
 delay chromeDelay              -- TODO ReaderMonad delay time  
 slotP p
 delay chromeDelay 
 press "C-<ret>"

open_application d = do
 openApplication$ mungeDictation d

bookmark_it d_ = do
 press "M-d"
 delay 1000
 press "<tab>"
 delay chromeDelay 
 press "<up>"
 maybe nothing bookmark_by_name d_

 where
 bookmark_by_name d = do 
   slotD d 
   delay 1000                    -- enough time to double check 
   replicateM_ 2 $ press "<ret>"

