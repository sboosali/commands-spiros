{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Macros where
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Emacs
import Commands.Plugins.Spiros.Emacs.Config

import Commands.Sugar.Press 
import Commands.Sugar.Alias
import           Commands.Etc
import Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX

import           Control.Applicative
import Data.List (intercalate)


newtype Macro = Macro Actions_
instance Show Macro where show (Macro a0) = showActions a0
instance Eq Macro where (==) (Macro a1) (Macro a2) = eqActions a1 a2
eqActions _a1 _a2 = False         -- TODO 

myMacros :: R z Macro
myMacros = 'myMacros <=> Macro <$> (myMacrosRHS0 <|> myMacrosRHS)

-- | macros without arguments
myMacrosRHS0 :: R z Actions_
myMacrosRHS0 = vocab
 [ ""-: nothing

 , "run again"-: do
   execute_extended_command
   press up
   press ret

 , "eval again"-: do
   eval_expression
   press up
   press ret

 , "to do"-: do
   insert "TODO "               -- TODO instance IsString Phrase' would overlap with instance IsString [a] 

 , "man"-: do                   -- short for "commands server"
   openApplication "Commands"   -- TODO make less stringly-typed
   delay 25
   move_window_down 
   delay 25
   switch_buffer (word2phrase' "shell2")

 , "next error"-: do
   move_window_down
   runEmacs "compilation-next-error"
   press ret

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

move_window_down = press S down

-- | macros with arguments
myMacrosRHS :: R z Actions_
myMacrosRHS = empty
 <|> align_regexp  <$ "align" <*> phrase_
 <|> switch_buffer <$ "buff"  <*> phrase_
 <|> multi_occur   <$ "occur" <*> phrase_
 <|> replace_with  <$"replace" <*> phrase_ <*"with" <*> phrase_
 <|> google_for <$ ("google") <*> (dictation-?-"")
 <|> search_regexp <$ "search" <*> (phrase_-?)
 <|> find_text <$ "find" <*> (phrase_-?-blankPhrase) -- TODO  No instance for (Data.String.IsString Phrase')
 <|> goto_line <$ "go" <*> number

align_regexp p' = do
 runEmacs "align-regexp"
 insertP p'

-- needs (setq confirm-nonexistent-file-or-buffer 'after-completion), which only switches to a buffer without prompt when that buffer already exists
switch_buffer p' = do
 press C 'x' >> press 'b'
 slotP p'

multi_occur p' = do
 runEmacs "multi-occur-in-matching-buffers"
 slot "."                       -- match all buffers 
 insertP p'                     -- match this regexp 

replace_with this that = do
 runEmacsWithP "replace-regexp" [this, that]

google_for (Dictation ws) = google (intercalate " " ws)

search_regexp = \case
 Nothing -> do
  press C 's'
 Just p -> do
  press C 's'
  insertP p

find_text p = do
 press M 'f'
 insertP p

goto_line :: Int -> Actions_
goto_line n = do
 press M 'g'    -- TODO generalize to AMonadAction
 -- press (n::Int) 
 slot (show n)

