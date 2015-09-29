{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts, GADTs, ConstraintKinds, FlexibleInstances, DataKinds            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Macros where
import Commands.Plugins.Spiros.Types
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Edit
import Commands.Plugins.Spiros.Emacs.Config
import           Commands.Plugins.Spiros.Shell
import Commands.Plugins.Spiros.Keys

import Commands.Sugar.Keys
import           Commands.Etc
import Commands.RHS.Types 
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import           Control.Applicative
import qualified Data.List as List
import Data.Function (on)
import Control.Arrow (second) 
import Control.Monad (replicateM_) 

-- default (Actions ())            -- ExtendedDefaultRules 


newtype Macro = Macro (Apply Rankable Actions_)
-- type Grammatical a = (Rankable a, Show a) -- , Eq a  -- LiberalTypeSynonyms not enough 

instance Show Macro where show (Macro _x) = "_"       -- TODO  showApply x
-- showApply :: (Show a) => Apply Show a -> String
-- showApply = show . runApply       -- TODO 

instance Eq Macro where (==) (Macro x) (Macro y) = eqApply x y
eqApply = eqActions `on` runApply    -- TODO also distinguish the constructors 
eqActions _a1 _a2 = False         -- TODO

runMacro (Macro f) = runApply f

-- | "freeze" function application, up to some arity. 
-- the arguments are existentially quantified, but can be constrained.
data Apply constraint r where
 A0 :: (constraint r)                                           
    =>                      r                       -> Apply constraint r    -- lol
 A1 :: (constraint a)                                           
    => (a ->                r) -> a                 -> Apply constraint r
 A2 :: (constraint a, constraint b)                             
    => (a -> b ->           r) -> a -> b            -> Apply constraint r
 A3 :: (constraint a, constraint b, constraint c)               
    => (a -> b -> c ->      r) -> a -> b -> c       -> Apply constraint r
 A4 :: (constraint a, constraint b, constraint c, constraint d) 
    => (a -> b -> c -> d -> r) -> a -> b -> c -> d  -> Apply constraint r

instance Rankable (Apply Rankable r) where rank = rankApply

-- arguments are existentially quantified 
rankApply :: Apply Rankable r -> Int
rankApply = \case
 A0 r         -> rank r
 A1 _ a       -> rank a
 A2 _ a b     -> safeAverage [rank a, rank b]
 A3 _ a b c   -> safeAverage [rank a, rank b, rank c]
 A4 _ a b c d -> safeAverage [rank a, rank b, rank c, rank d]

runApply :: Apply constraint r -> r
runApply = \case
 A0 f         -> f
 A1 f a       -> f a
 A2 f a b     -> f a b
 A3 f a b c   -> f a b c
 A4 f a b c d -> f a b c d



-- ================================================================ --

alt_tab = do
 press_ "M-<tab>"
 press_ "<ret>" 

move_window_down = press_ "S-<down>"

move_window_up = press_ "S-<up>"

toggle_clipboard_history = press_ "A-<spc>"               -- NOTE Alfred
toggle_alfred = press_ "M-<spc>"               -- NOTE Alfred

open_pad = do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_down
   delay 100
   switch_buffer (word2phrase "*pad*") -- TODO make variable 
   delay 100

emacs_reach_shell = do
   move_window_down 
   delay 25
   switch_buffer (word2phrase "*shell*")
   delay 25
   window_bottom                -- TODO make typed/generic like runEdit
   runMove (MoveTo Beginning Line)
   runEdit (Edit Delete Forwards Line)

reach_youtube = do
   openApplication "Google Chrome" -- TODO make variable 
   switch_tab (word2phrase "YouTube.com")

youtube_toggle_fullscreen = press_ "S-f"

youtube_toggle_sound = do
   app <- currentApplication
   reach_youtube
   press_ "M-<up>"
   delay chromeDelay 
   youtube_toggle_fullscreen 
   delay chromeDelay
   press_ "k"      -- pauses the video, somehow  
   delay chromeDelay 
   youtube_toggle_fullscreen 
   delay 2000
   openApplication app


-- ================================================================ --

myMacros :: R z Macro
myMacros = 'myMacros
 <=> (Macro . A0) <$> myMacrosRHS0
 <|> Macro        <$> myMacrosRHS

-- | macros without arguments
myMacrosRHS0 :: R z Actions_
myMacrosRHS0 = myAliases <|> __inlineRHS__(myOrdinals) <|> myApps <|> vocab
 [ ""-: nothing

 , "run again"-: do
   execute_extended_command
   press_ "<up>"
   press_ "<ret>"

 , "eval again"-: do
   eval_expression
   press_ "<up>"
   press_ "<ret>"

 , "to do"-: do
   insert "TODO "               -- TODO instance IsString Phrase' would overlap with instance IsString [a] 

 , "voice"-: do                   -- short for "commands server"
   openApplication "Terminal"   -- TODO make less stringly-typed
   press_ "<del>" 

 , "voice build"-: do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   emacs_reach_shell
   slot "cabal build server"

 , "next error"-: do
   move_window_down
   runEmacs "compilation-next-error"
   press_ "C-l"
   press_ "C-l"
   press_ "<ret>" 

 , "macro"-: do   -- TODO LOL 
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_up
   delay 100
   switch_buffer (word2phrase "Macro.hs") -- TODO make variable 
   delay 100

   press_ "M-<up>"
   press_ "C-s" >> slot " , \"\"-: do" >> replicateM_ 6 (press_ "<left>") 
   -- press_ "C-s" >> slot " , \"\"-: do" >> press_ "<left><right>"
   -- press_ "C-s" >> slot "\""           >> press_ "<left><right>"

 , "shortcut"-: do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_up
   delay 100
   switch_buffer (word2phrase "Shortcut.hs") -- TODO make variable 
   delay 100

 -- TODO make "C-x C-y" the commands key prefix
 , "copy register"-: do
   press_ "C-x C-y r c" -- nonstandard: my-register 

 , "paste register"-: do
   press_ "C-x C-y r v" -- nonstandard: my-register 

 , "clear register"-: do
   press_ "C-x C-y r d" -- nonstandard: my-register 

 , "magic"-: do
   runEmacs "magit-status"

 , "music"-: do
   reach_youtube

 , "pause"-: do
   youtube_toggle_sound

 , "play"-: do
   youtube_toggle_sound

 , "fun"-: do
   alt_tab

 , "check"-: do
   runEmacs "compile"
   -- press_ keymap >> press_ "c"

 , "exec again"-: do
   press_ "S-<down>"
   press_ "M-<down>"
   press_ "C-<up>"
   press_ "<ret>"

 , "phonetic alphabet"-: do
   insertByClipboard$ (List.intercalate "\n" . map fst) phoneticAlphabet 

 , "bookmark"-: do
   press_ "M-d"
   delay 1000
   press_ "<tab>"
   delay chromeDelay 
   press_ "<up>"
   -- then say "two ret" or click away 

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
myAliases :: R z Actions_             -- TODO String
myAliases = vocab$ fmap (second sendText) -- TODO embed into any phrase. in grammar itself? or, with less accuracy, just in phrase runner 
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

myOrdinals :: R z Actions_
myOrdinals = 'myOrdinals <=> runOrdinalKeypress <$> (__inlineRHS__(ordinalDigit))
 -- __inlineRHS__ because: we want myMacrosRHS0 to be flattened into a vocabulary
 -- the cast is safe because: ordinalDigit is between zero and nine, inclusive

-- | run an ordinal as a keypress.
-- @runOrdinalKeypress (Ordinal 3)@ is like @press "M-3"@. 
runOrdinalKeypress :: Ordinal -> Actions_
runOrdinalKeypress
 = uncurry sendKeyPress
 . ordinal2keypress

ordinal2keypress :: Ordinal -> KeyPress
ordinal2keypress 
 = addMod CommandMod
 . (either __BUG__ id)
 . digit2keypress
 . unOrdinal

myApps :: R z Actions_
myApps = vocab $ fmap (second openApplication)  -- TODO make less stringly-typed
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




-- ================================================================ --

-- | macros with arguments
myMacrosRHS :: R z (Apply Rankable Actions_)
myMacrosRHS = empty
 <|> A1 align_regexp            <$ "align"     <*> phrase
 <|> A1 switch_buffer           <$ "buffer"    <*> phrase
 <|> A1 multi_occur             <$ "occur"     <*> phrase
 <|> A2 replace_with            <$ "replace"   <*> phrase <*"with" <*> phrase
 <|> A1 google_for              <$ "google"    <*> (phrase-?-"")
 <|> A1 search_regexp           <$ "search"    <*> (phrase-?)
 <|> A1 find_text               <$ "discover"  <*> (phrase-?-"")
 <|> A1 goto_line               <$ "go"        <*> number
 <|> A1 comment_with            <$ "comment"   <*> (phrase-?)
 <|> A1 write_to_pad            <$ "scribble"  <*> (phrase-?)
 <|> A1 run_shell               <$ "shell"     <*> (shell-|-(phrase-?))
 <|> A1 query_clipboard_history <$ "clipboard" <*> ((ordinalDigit-|-phrase)-?)
 <|> A1 query_alfred            <$ "Alfred"    <*> (phrase-?)
 <|> A1 switch_tab              <$ "tab"       <*> (phrase-?-"")
 <|> A1 visit_site              <$ "visit"     <*> (phrase-?-"")
 <|> A1 google_click_link       <$ "link"      <*> phrase
-- TODO keep a elisp expression that aligns the block of code, when eval-last-sexp
-- TODO <\$ <\*>

-- we need the Apply constructors to delay function application, which allows the parser to disambiguate by ranking the arguments, still unapplied until execution

align_regexp p = do
 runEmacs "align-regexp"
 insertP p

-- needs (setq confirm-nonexistent-file-or-buffer 'after-completion), which only switches to a buffer without prompt when that buffer already exists
switch_buffer p = do
 press_ "C-x b"
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
  press_ "C-s"
  maybe nothing insertP p

find_text p = do
 press_ "M-f"
 delay browserDelay
 insertP p

goto_line :: Int -> Actions_
goto_line n = do
 press_ "M-g"    -- TODO generalize to AMonadAction_, as well as PressFun https://github.com/AJFarmar/haskell-polyvariadic
 -- press (n::Int) 
 slot (show n)

comment_with :: Maybe Phrase -> Actions_
comment_with p = do
 press_ "M-;"
 maybe nothing insertP p

write_to_pad p = do
 open_pad
 press_ "<ret>" 
 maybe nothing insertP p

run_shell (Left s) = do
 emacs_reach_shell
 runShell s
run_shell (Right p) = do
 emacs_reach_shell
 maybe nothing insertP p


query_clipboard_history :: Maybe (Either Ordinal Phrase) -> Actions_
query_clipboard_history Nothing = do
 toggle_clipboard_history
query_clipboard_history (Just (Left n)) = do 
 toggle_clipboard_history
 delay 500
 runOrdinalKeypress n
query_clipboard_history (Just (Right p)) = do
 toggle_clipboard_history
 delay 500
 insertP p

query_alfred p = do
 toggle_alfred 
 delay 500
 maybe nothing insertP p

switch_tab p = do
 press_ "A-t"                    -- needs Tab Ahead chrome extension 
 delay chromeDelay 
 slotP p

visit_site p = do
 openApplication "Google Chrome"   -- TODO make variable 
 press_ "M-t"
 delay chromeDelay 
 slotP p

-- http://superuser.com/questions/170353/chrome-selecting-a-link-by-doing-search-on-its-text
google_click_link p = do
 press_ "M-f"
 delay chromeDelay              -- TODO ReaderMonad delay time  
 slotP p
 delay chromeDelay 
 press_ "C-<ret>"

