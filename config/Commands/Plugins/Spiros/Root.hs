{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms, PostfixOperators, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, OverloadedStrings, TupleSections            #-}
{-# LANGUAGE ViewPatterns                                                   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Root where
import           Commands.Plugins.Spiros.Root.Types 
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Macros
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Shortcut
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
import  Commands.Plugins.Spiros.Keys

import           Commands.Mixins.DNS13OSX9

import           Control.Applicative


roots :: R z Roots
roots = 'roots
 <=> Frozen <$ (token"freeze") <*> root --TODO recursion
 <|> Ambiguous <$ (token"explicate") <*> root --TODO recursion
 <|> Root_ <$> root

root :: R z Root
root = 'root <=> empty
 <|> Acts_      <$> (acts-++)
 <|> Emacs_     <$> (number-?-1) <*> emacs
 <|> Shortcut_  <$> (number-?-1) <*> myShortcuts
 <|> Macro_     <$> (number-?-1) <*> myMacros
 <|> Shell_     <$>                  shell
 <|> Dictation_ <$  (token"say")  <*> dictation
 --TODO <|> Phrase_    <$  (token"pray") <*> phrase
 <|> Phrase_    <$> phrase  -- must be last, phrase falls back to wildcard.

acts = 'acts
 <=> ActsRW <$> (number-?-1) <*> act

act = 'act <=> empty     -- boilerplate (mostly)
 <|> KeyRiff_ <$> keyriff
 --TODO <|> Click_   <$> click
 <|> Edit_    <$> edit
 <|> Move_    <$> move

-- data Acts
--  = ActRW Int Act   -- ^ actions can be chained (RW means read/write) 
--  | ActRO Act   -- ^ idempotent(ish) actions don't need immediate repetition (RO means read-in only) .

-- acts = 'acts
--  <=> ActRW <$> (number-?-1) <*> actRW
--  <|> ActRO <$> actRO

-- actRO = 'actRO <=> empty -- TODO

-- actRW = 'actRW <=> empty
--  <|> act

click = 'click <=>
 Click <$> (times-?-Single) <*> (button-?-LeftButton) <$ "click"

times = enumGrammar

button = qualifiedGrammar


