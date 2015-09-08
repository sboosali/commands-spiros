{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Macros where
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Emacs

import Commands.Mixins.DNS13OSX9
import           Commands.Sugar.Press
import           Commands.Backends.OSX

import           Control.Applicative


newtype Macro = Macro Actions_
instance Show Macro where show (Macro a0) = showActions a0
instance Eq Macro where (==) (Macro a1) (Macro a2) = eqActions a1 a2
eqActions _a1 _a2 = False         -- TODO 


myMacros :: R z Macro
myMacros = 'myMacros <=> Macro <$> myMacrosRHS

myMacrosRHS :: R z Actions_ 
myMacrosRHS = empty
 <|> align_regexp <$ "align" <*> phrase_
 <|> select_buffer <$ "buff" <*> phrase_
 <|> multi_occur <$ "occur" <*> phrase_

align_regexp p' = do
 runEmacsWait "align-regexp"
 insertP p'

select_buffer p' = do
 press C 'x' >> press 'b'
 insertP p'

multi_occur p' = do
 runEmacsWait "multi-occur-in-matching-buffers"
 slot "."                       -- match all buffers 
 insertP p'                     -- match this regexp 

