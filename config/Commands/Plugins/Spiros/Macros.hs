{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase       #-}
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


newtype Macro = Macro Actions_
instance Show Macro where show (Macro a0) = showActions a0
instance Eq Macro where (==) (Macro a1) (Macro a2) = eqActions a1 a2
eqActions _a1 _a2 = False         -- TODO 


myMacros :: R z Macro
myMacros = 'myMacros <=> Macro <$> (myMacrosRHS0 <|> myMacrosRHS)

-- | macros with arguments
myMacrosRHS :: R z Actions_
myMacrosRHS = empty
 <|> align_regexp  <$ "align" <*> phrase_
 <|> switch_buffer <$ "buff"  <*> phrase_
 <|> multi_occur   <$ "occur" <*> phrase_
 <|> replace_with  <$"replace" <*> phrase_ <*"with" <*> phrase_

align_regexp p' = do
 runEmacs "align-regexp"
 insertP p'

-- (setq confirm-nonexistent-file-or-buffer 'after-completion) only switches to a buffer without prompt when that buffer already exists
switch_buffer p' = do
 press C 'x' >> press 'b'
 slotP p'

multi_occur p' = do
 runEmacs "multi-occur-in-matching-buffers"
 slot "."                       -- match all buffers 
 insertP p'                     -- match this regexp 

replace_with this that = do
 runEmacsWithP "replace-regexp" [this, that]

-- | macros without arguments
myMacrosRHS0 :: R z Actions_
myMacrosRHS0 = vocab
 [ ""-: nothing

 , "run again"-: do
   execute_extended_command
   press C up
   press ret

 , "eval again"-: do
   eval_expression
   press C up
   press ret

 , "to do"-: do
   insert "TODO"

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
