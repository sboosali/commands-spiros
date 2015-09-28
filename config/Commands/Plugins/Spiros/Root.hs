{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ImplicitParams, LambdaCase  #-}
{-# LANGUAGE LiberalTypeSynonyms, NamedFieldPuns, PartialTypeSignatures     #-}
{-# LANGUAGE PatternSynonyms, PostfixOperators, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TupleSections            #-}
{-# LANGUAGE ViewPatterns                                                   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Root where
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Emacs.Config
import           Commands.Plugins.Spiros.Macros
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Shortcut
import           Commands.Plugins.Spiros.Shell
import           Commands.Plugins.Spiros.Edit
import Commands.Plugins.Spiros.Keys

import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Mixins.DNS13OSX9
import Commands.Sugar.Keys

import           Data.Typeable
import           Numeric.Natural ()

import           Control.Applicative hiding (many, optional)
-- import           Control.Parallel
import qualified Data.List as List
import           Prelude hiding (foldl, foldr1)
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe


data Roots
 = Frozen Root
 | Ambiguous Root
 | Root_ Root
 deriving (Show,Eq)

roots :: R z Roots
roots = 'roots
 <=> Frozen <$ (token"freeze") <*> root --TODO recursion
 <|> Ambiguous <$ (token"explicate") <*> root --TODO recursion
 <|> Root_ <$> root

data Root
 = Acts_       [Acts]         -- ^ chained and repeated
 | Macro_      Number  Macro  -- ^ repeated 
 | Shortcut_   Number  Shortcut  -- ^ repeated 
 | Shell_              Shell  -- ^
 | Emacs_      Number  Emacs  -- ^ repeated 
 | Dictation_  Dictation      -- ^ 
 | Phrase_     Phrase        -- ^ 
 deriving (Show,Eq)

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

data Acts
 = ActsRW Int Act   -- ^ read/write actions
 deriving (Show,Eq)

acts = 'acts
 <=> ActsRW <$> (number-?-1) <*> act

data Act
 = KeyRiff_ KeyRiff
 --TODO | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Eq)

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

data Click = Click Times Button deriving (Show,Eq)
click = 'click <=>
 Click <$> (times-?-Single) <*> (button-?-LeftButton) # "click"

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = enumGrammar

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Enum,Typeable)
button = qualifiedGrammar


-- ================================================================ --

rootsCommand :: C z Roots
rootsCommand = Command roots bestRoots runRoots

rootsParser :: RULED EarleyParser s Roots
rootsParser = EarleyParser rootsProd bestRoots

rootsProd :: RULED EarleyProd s Roots
rootsProd = unsafePerformIO$ unsafeSTToIO$ de'deriveParserObservedSharing roots

bestRoots = argmax rankRoots

rankRoots = \case                --TODO fold over every field of every case, normalizing each case
 Frozen r -> highRank + rankRoot r
 Ambiguous r -> highRank + rankRoot r
 Root_ r   -> rankRoot r

rankRoot = \case
 Acts_ ass            -> safeAverage (fmap rankActs ass) 
 Macro_ _i (Macro f) -> highRank + rankApply f
 Shortcut_ _i _s -> highRank
 Shell_ s           -> highRank + rankShell s
 Emacs_ _i e        -> highRank + rankEmacs e
 Dictation_ _d       -> highRank
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
 Frozen r -> insert (show r)
 Ambiguous _r -> nothing         -- TODO needs magic server actions , which needs a more general monad stack 
 Root_ r  -> runRoot context r
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

runRoot context = \case
 Acts_ ass     -> traverse_ (runActs context) ass      -- no delay 
 Macro_ n f    -> runRepeat (contextualDelay context) n (runMacro f)
 Shortcut_ n s -> runRepeat (contextualDelay context) n (runShortcut s) 
 Shell_ s      -> runShell s
 Emacs_ n e   -> onlyWhen isEmacs context $ runRepeat emacsDelay n (runEmacs_ e) 
 Dictation_ d -> runDictation d
 Phrase_ p    -> runPhrase context p

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

runPhrase _context p = do
 insert =<< munge p
 insert " "

runPhraseByClipboard _context p = do
 insertByClipboard =<< munge p
 insert " "

runDictation = \case
 Dictation ws -> insert (List.intercalate " " ws)

