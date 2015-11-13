{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ImplicitParams, LambdaCase  #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms, PostfixOperators, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, OverloadedStrings, TupleSections            #-}
{-# LANGUAGE ViewPatterns                                                   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Root.Run where 
import           Commands.Plugins.Spiros.Root.Types 
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Types (isEmacs) 
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Macros.Types
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
import           Commands.Plugins.Spiros.Shortcut.Types 

import           Commands.Backends.OSX
import Commands.Sugar.Keys

-- import           Control.Parallel
import           Data.Maybe 


bestRoots = argmax rankRoots

rankRoots = \case                --TODO fold over every field of every case, normalizing each case
 Frozen _ r -> highRank + rankRoot r
 Ambiguous r -> highRank + rankRoot r
 Root_ r   -> rankRoot r

-- prioritize Action over Macro
-- ideally, prioritize "more specific" over "less specific" matches 
rankRoot = \case
 Acts_ ass            -> 4 * highRank + safeAverage (fmap rankActs ass)
   -- NOTE "google word" now matches the Action, not the Macro 
 Macro_ _i m        -> 3 * highRank + rankMacro m
 Shortcut_ _i _s    -> 3 * highRank
 Shell_ s           -> highRank + rankShell s
 Emacs_ _i e        -> highRank + rankEmacs e
 Letters_ _l         -> highRank + 1 
 Dictation_ _d       -> highRank + 0
 Phrase_    p        -> rankPhrase p

rankActs = \case
 ActsRW _i a -> rankAct a

rankAct = \case
 KeyRiff_ _kr -> highRank
 -- TODOClick_ _c    -> defaultRank
 Edit_ e      -> defaultRank + rankEdit e
 Move_ m     -> defaultRank + rankMove m



-- ================================================================ --

runRoots context = \case
 Frozen _ _ -> nothing           -- TODO needs magic server actions , which needs a more general monad stack 
 Ambiguous _ -> nothing         -- TODO needs magic server actions , which needs a more general monad stack 
 Root_ r  -> runRoot context r
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

runRoot context = \case
 Acts_ ass     -> traverse_ (runActs context) ass      -- no delay 
 Macro_ n f    -> runRepeat (contextualDelay context) n (runMacro f)
 Shortcut_ n s -> runRepeat (contextualDelay context) n (runShortcut s) 
 Shell_ s      -> runShell s
 Emacs_ n e   -> onlyWhen isEmacs context $ runRepeat emacsDelay n (runEmacs_ e) 
 Letters_ l   -> runLetters l
 Dictation_ d -> runDictationTop context d 
 Phrase_ p    -> runPhraseTop context p

contextualDelay = \case
 (isEmacs   -> Just{}) -> emacsDelay
 (isBrowser -> Just{}) -> browserDelay
 _                     -> defaultDelay

runActs context = \case
 ActsRW n a -> runRepeat emacsDelay n (runAct context a)

runAct context = \case
 KeyRiff_ kr -> runKeyRiff kr
 --TODO Click_ _c   -> nothing
 Edit_ a     -> onlyWhen isEmacs context $ editEmacs a
 Move_ a     -> onlyWhen isEmacs context $ moveEmacs a

-- | 
runPhraseByClipboard _context p = do
 s <- (<>" ") <$> munge p 
 insertByClipboard s 

-- | top level TODO actual spacing
runPhraseTop context p = do
 s <- (<>" ") <$> munge p 
 insertGiven context s 

-- | top level TODO actual spacing
runDictationTop context d = do
 let s = mungeDictation (spacedDictation d)
 insertGiven context s 

-- | 
insertGiven context s = do 
 if (isJust . isEmacs) context 
 then insertEmacs s
 else insertDefault s

insertDefault = insert 

insertEmacs = insertMarking 
-- insertEmacs = insertHighlighting

