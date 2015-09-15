{-# LANGUAGE  TemplateHaskell, OverloadedStrings, PostfixOperators, RankNTypes, LambdaCase, FlexibleContexts, GADTs, ConstraintKinds, FlexibleInstances, DataKinds            #-}
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
-- import Data.List (intercalate)
import Data.Function (on)

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

myMacros :: R z Macro
myMacros = 'myMacros
 <=> (Macro . A0) <$> myMacrosRHS0
 <|> Macro        <$> myMacrosRHS

-- | macros without arguments
myAliases :: R z String
myAliases = vocab
 [ ""-: ""
 , "arrow"-: "->"
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
 , ""-: ""
 , ""-: ""
 ]

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
   switch_buffer (word2phrase' "*shell2*")
   delay 25
   window_bottom 

 , "make"-: do
   openApplication "Commands"   -- TODO make variable 
   delay 100
   move_window_down 
   delay 25
   switch_buffer (word2phrase' "*shell*") -- TODO make variable 
   delay 100
   press M down
   press C 'a'
   press C 'k'
   slot "make build"

 , "next error"-: do
   move_window_down
   runEmacs "compilation-next-error"
   press M 'l'
   press M 'l'
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
myMacrosRHS :: R z (Apply Rankable Actions_)
myMacrosRHS = empty
 <|> A1 align_regexp  <$ "align"    <*> phrase_
 <|> A1 switch_buffer <$ "buff"     <*> phrase_
 <|> A1 multi_occur   <$ "occur"    <*> phrase_
 <|> A2 replace_with  <$"replace"   <*> phrase_ <*"with" <*> phrase_
 <|> A1 google_for    <$ "goo" <*> (phrase_-?-blankPhrase)
 <|> A1 search_regexp <$ "search"   <*> (phrase_-?)
 <|> A1 find_text     <$ "find"     <*> (phrase_-?-blankPhrase) -- TODO  No instance for (Data.String.IsString Phrase')
 <|> A1 goto_line     <$ "go"       <*> number
 <|> A1 comment_with  <$ "comment"  <*> (phrase_-?)

-- we need the Apply constructors to delay function application, which allows the parser to disambiguate by ranking the arguments, still unapplied until execution

align_regexp p = do
 runEmacs "align-regexp"
 insertP p

-- needs (setq confirm-nonexistent-file-or-buffer 'after-completion), which only switches to a buffer without prompt when that buffer already exists
switch_buffer p = do
 press C 'x' >> press 'b'
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

search_regexp = \case
 Nothing -> do
  press C 's'
 Just p -> do
  press C 's'
  insertP p

find_text p = do
 press M 'f'
 delay browserDelay
 insertP p

goto_line :: Int -> Actions_
goto_line n = do
 press M 'g'    -- TODO generalize to AMonadAction_, as well as PressFun https://github.com/AJFarmar/haskell-polyvariadic
 -- press (n::Int) 
 slot (show n)

comment_with :: Maybe Phrase' -> Actions_
comment_with p = do
 press M ';'
 maybe nothing insertP p

