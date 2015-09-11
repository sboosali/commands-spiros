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

import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
-- import           Commands.Munging
import           Commands.Plugins.Example.Keys
import           Commands.Plugins.Example.Spacing
import           Commands.Sugar.Alias
import           Commands.Sugar.Press

import           Control.Concurrent.Async
import           Control.Lens hiding (from, ( # ))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Typeable
import           Numeric.Natural ()

import           Control.Applicative hiding (many, optional)
import           Control.Monad (replicateM_, (>=>))
-- import           Control.Parallel
import           Data.Foldable                         (Foldable (..), asum,
                                                        traverse_)
import qualified Data.List as List
-- import           Data.Monoid
import           Prelude hiding (foldl, foldr1)
import           System.Timeout (timeout)
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe


data Roots
 = Frozen Root
 | Root_ Root
 deriving (Show,Eq)

roots :: R z Roots
roots = 'roots
 <=> Frozen <$ (token"freeze") <*> root --TODO recursion
 <|> Root_ <$> root

data Root
  =  Acts_       [Acts]         -- ^ chained and repeated
  |  Macro_      Number  Macro  -- ^ repeated 
  |  Shortcut_   Number  Shortcut  -- ^ repeated 
  |  Emacs_      Number  Emacs  -- ^ repeated 
  |  Dictation_  Dictation      -- ^ 
  |  Phrase_     Phrase'        -- ^ 
 deriving (Show,Eq)

root :: R z Root
root = 'root <=> empty
 <|> Acts_      <$> (acts-++)
 <|> Emacs_     <$> (number-?-1) <*> emacs
 <|> Shortcut_  <$> (number-?-1) <*> myShortcuts
 <|> Macro_     <$> (number-?-1) <*> myMacros
 <|> Dictation_ <$  (token"say") <*> dictation
 <|> Phrase_    <$> phrase_  -- must be last, phrase falls back to wildcard.

data Acts
 = ActsRW Int Act   -- ^ read/write actions
 deriving (Show,Eq)

acts = 'acts
 <=> ActsRW <$> (number-?-1) <*> act

data Act
 = KeyRiff_ KeyRiff
 | Click_ Click
 | Edit_ Edit
 | Move_ Move
 deriving (Show,Eq)

act = 'act <=> empty     -- boilerplate (mostly)
 <|> KeyRiff_ <$> keyriff
 <|> Click_   <$> click
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

data Move
 = Move Direction Region
 | MoveTo Endpoint Region
 deriving (Show,Eq,Ord)
move = 'move
 <=> Move   <#> direction # region
 <|> MoveTo <#> endpoint # region
-- TODO scrap this boilerplate.
--
-- can't scrap it with GHC.generics because the grammars are values not instance methods.
-- but maybe we can:
 -- with singleton stuff (?)
 -- with reflection, by building a new instance at runtime?
 -- given all children derive Generic and/or Data, by building a new generic grammar for each
 -- given all children derive Grammatical. and DeriveAnyClass makes this easier! deriving (Generic, Grammatical)
--
-- we could scrap it with TemplateHaskell if we were really wanted to, to gain that edit-once property, lowercasing the type to get the value, but I don't want to.
-- move = defaultRule 'move
-- defaultRule "move" OR spliceRule ''Move
--

-- | Slice and Direction both have too many values.
data Endpoint = Beginning | Ending deriving (Bounded,Enum,Eq,Ord,Read,Show)
endpoint = 'endpoint
 <=> Beginning <#> "beg"
 <|> Ending    <#> "end"

-- | orthogonal directions in three-dimensional space. @... <=> Up_ <#> "up" <|> ...@
data Direction = Up_ | Down_ | Left_ | Right_ | In_ | Out_  deriving (Show,Eq,Ord,Enum,Typeable)
direction = tidyGrammar
-- direction = transformedGrammar (filter (/= '_'))
-- direction = qualifiedGrammarWith "_"

{- | slice the region between the cursor and the 'Slice'. induces a string.
-}
data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum,Typeable)
slice = 'slice <=> vocab
 [ "whole"-:Whole
 , "back"-: Backwards
 , "fuss"-: Forwards
 ]
 -- "for" would be homophone with "four", while both Positive and Slice can be the prefix (i.e. competing for the same recognition).
-- "four kill for word" should be [4 Kill Forwards Word] not [4 Kill Forwards Line, 4 Sel Whole Word]
-- "four kill fuss word" is unambiguously [4 Kill Forwards Word]


data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)

edit = 'edit
 <=> Edit Cut Forwards Line <#> "kill"
     -- i.e. "kill" -> "kill for line", not "kill whole that"
 <|> Edit <$> action              <*> (slice -?- Whole) <*> (region -?- That)
    -- e.g. "cop" or "cop that" or "cop whole" -> "cop whole that"
 <|> Edit <$> (action -?- Select) <*> slice             <*> region
    -- e.g. "for line" -> "sel for line"

 -- the "kill" case is why I abandoned parsec: it didn't backtrack sufficiently. we want:
 -- "cop" -> Edit Copy Whole That
 -- "kill" -> Edit Cut Forwards Line, not Edit Cut Whole That
 -- "kill for line" -> Edit Cut Forwards Line, not {unexpected 'f', expecting end of input}

-- TODO maybe RHS should have access to a configuration environment? Oh my.
-- could also provide the keyword (i.e. only literals) feature, rather than forcing it on the parser.


data Action
 = Select                       -- read-only.
 | Copy                         -- read-only.
 | Cut                          -- read/write.
 | Delete                       -- read/write.
 | Transpose                    -- read/write.
 | Google                       -- read-only.
 deriving (Show,Eq,Ord,Typeable)

action = 'action <=> empty
 <|> Select      <#> "sell"
 <|> Copy        <#> "cop"
 <|> Cut         <#> "kill"
 <|> Delete      <#> "del"
 <|> Transpose   <#> "trans"
 <|> Google      <#> "goo"



data Region
 = That

 | Character
 | Word_                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all two 'Word_'s
 | Token                        -- ^ e.g. @"camelCase"@, @"lisp-case"@, @"snake_case"@ are all one 'Token's
 | Group                        -- ^ 'Bracket's delimit 'Group's (e.g. @"(...)"@ or @"<...>"@ or @"[|...|]"@)
 | Line
 | Rectangle
 | Block
 | Page
 | Screen
 | Everything

 | Definition
 | Function_
 | Reference
 | Structure
 deriving (Show,Eq,Ord,Enum,Typeable)

region = 'region
 <=> That       <#> "that"
 <|> Character  <#> "char"
 <|> Word_      <#> "word"
 <|> Token      <#> "toke"
 <|> Group      <#> "group"
 <|> Line       <#> "line"
 <|> Rectangle  <#> "wreck"
 <|> Block      <#> "block"
 <|> Page       <#> "page"
 <|> Screen     <#> "screen"
 <|> Everything <#> "all"
 <|> Definition <#> "def"
 <|> Function_  <#> "fun"
 <|> Reference  <#> "ref"
 <|> Structure  <#> "struct"


data Click = Click Times Button deriving (Show,Eq)
click = 'click <=>
 Click <$> (times-?-Single) <*> (button-?-LeftButton) # "click"

data Times = Single | Double | Triple deriving (Show,Eq,Enum,Typeable)
times = enumGrammar

data Button = LeftButton | MiddleButton | RightButton deriving (Show,Eq,Enum,Typeable)
button = qualifiedGrammar

positive = 'positive
 <=> Positive <$> (asum . fmap int) [1..9]
 -- <|> Positive 0 <#> "zero"
 -- <|> Positive 1 <#> "one"
 <|> Positive 2 <#> "two"
 <|> Positive 3 <#> "three"
 <|> Positive 4 <#> "four"
 <|> Positive 5 <#> "five"
 <|> Positive 6 <#> "six"
 <|> Positive 7 <#> "seven"
 <|> Positive 8 <#> "eight"
 <|> Positive 9 <#> "nine"
 <|> Positive 10 <#> "ten"


{-

 Up_
 Down_
 Left_
 Right_
 In_
 Out_

 Select
 Beginning
 End
 Copy
 Cut
 Delete
 Transpose
 Google

 That
 Character
 Word_
 Token
 Group
 Line
 Rectangle
 Block
 Page
 Screen
 Everything
 Definition
 Function_
 Reference
 Structure

-}


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
 Root_ r   -> rankRoot r

rankRoot = \case
 Acts_ ass            -> safeAverage (fmap rankActs ass) 
 Macro_ _i (Macro f) -> highRank + rankApply f
 Shortcut_ _i _s -> highRank
 Emacs_ _i _m        -> highRank
 Dictation_ _d       -> highRank
 Phrase_    p        -> rankPhrase p

rankActs = \case
 ActsRW _i a -> rankAct a

rankAct = \case
 KeyRiff_ _kr -> highRank
 Click_ _c    -> defaultRank
 Edit_ _e     -> defaultRank
 Move_ _m     -> defaultRank

runRoots context = \case
 Frozen r -> insert (show r)
 Root_ r  -> runRoot context r
--    _ -> pretty print the tree of commands, both as a tree and as the flat recognition,
--  (inverse of parsing), rather than executing. For debugging/practicing, and maybe for batching.

runRoot context = \case
 Acts_ ass     -> onlyWhen isEmacs context $ traverse_ runActs ass      -- no delay 
 Macro_ n f    -> runRepeat (contextualDelay context) n (runMacro f)
 Shortcut_ n s -> runRepeat (contextualDelay context) n (runShortcut s) 
 Emacs_ n e   -> onlyWhen isEmacs context $ runRepeat emacsDelay n (runEmacs_ e) 
 Dictation_ d -> runDictation d
 Phrase_ p    -> runPhrase p

contextualDelay = \case
 (isEmacs   -> Just{}) -> emacsDelay
 (isBrowser -> Just{}) -> browserDelay
 _                     -> defaultDelay

runActs = \case
 ActsRW n a -> runRepeat emacsDelay n (runAct a)

runAct = \case
 KeyRiff_ kr -> runKeyRiff kr
 Click_ _c    -> nothing
 Edit_ a     -> editEmacs a
 Move_ a     -> moveEmacs a

runPhrase p = do
 insert =<< munge p
 insert " "

runDictation = \case
 Dictation ws -> insert (List.intercalate " " ws)

-- the indirection (i.e. @data 'Move'@, not just a @String@) makes it easy to reinterpret in many ways (e.g. moveEmacs, moveIntelliJ, moveChromd , etc).
moveEmacs :: Move -> Actions ()
moveEmacs = \case

 Move Left_ Character  -> press C 'b'
 Move Right_ Character -> press C 'f'

 Move Left_ Word_      -> press M 'b'
 Move Right_ Word_     -> press M 'f'

 Move Left_ Group      -> press C M 'b'
 Move Right_ Group     -> press C M 'f'

 Move Up_ Line         -> press C 'p'
 Move Down_ Line       -> press C 'n'

 Move Up_ Block        -> press C up
 Move Down_ Block      -> press C down

 Move Up_ Screen       -> runEmacs "scroll-up-command"
 Move Down_ Screen     -> press C 'v'

 Move Up_ Page         -> runEmacs "backward-page"
 Move Down_ Page       -> runEmacs "forward-page"

 MoveTo Beginning Line       -> press C 'a'
 MoveTo Ending    Line       -> press C 'e'

 MoveTo Beginning Everything -> press M up
 MoveTo Ending    Everything -> press M down

 -- Move -> press
 -- MoveTo -> press
 _ -> nothing

-- gets the given region of text from Emacs
selected :: Slice -> Region -> Actions String
selected s r = do
 -- editEmacs (Edit Select s r)
 select r s
 copy

select :: Region -> Slice -> Actions ()
select That = \case
 _ -> nothing     -- (should be) already selected
-- select Character = \case
--  _ -> nothing
select r = \case
 Whole     -> beg_of r >> mark >> end_of r
 Backwards -> mark >> beg_of r
 Forwards  -> mark >> end_of r

{-

idempotent means

idempotent means unchainable.
instead of [3 select word], how about [select 3 word]
where the first selection is idempotent, and the next two Move Right.
In Emacs, this preserves the mark.



-}
-- | should be idempotent (in Emacs, not Haskell).
beg_of :: Region -> Actions ()
beg_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-beginning))"
 Character  -> nothing
 Word_      -> evalEmacs "(beginning-of-thing 'word)"
 Group      -> evalEmacs "(beginning-of-thing 'list)"
 Line       -> press C 'a'
 Block      -> evalEmacs "(beginning-of-thing 'block)"
 Page       -> evalEmacs "(beginning-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-start))"
 Everything -> runEmacs "beginning-of-buffer"
 _          -> nothing

-- | should be idempotent (in Emacs, not Haskell).
end_of :: Region -> Actions ()
end_of = \case
 -- runEmacs
 That       -> evalEmacs "(goto-char (region-end))"
 Character  -> nothing          -- [press C f] is not idempotent, but [nothing] fails on [beg_of r >> mark >> end_of r]
 Word_      -> evalEmacs "(end-of-thing 'word)"
 Group      -> evalEmacs "(end-of-thing 'list)"
 Line       -> press C 'e'
 Block      -> evalEmacs "(end-of-thing 'block)" -- non-standard: expects forward-block
 Page       -> evalEmacs "(end-of-thing 'page)"
 Screen     -> evalEmacs "(goto-char (window-end))"
 Everything -> runEmacs "end-of-buffer"
 _          -> nothing

-- | vim's composeability would keep the number of cases linear (not quadratic in 'Action's times 'Region's).
-- in Emacs, we can use <http://www.emacswiki.org/emacs/ThingAtPoint thingatpt.el>.
editEmacs :: Edit -> Actions ()
editEmacs = \case

 Edit Select Whole Line -> do -- special behavior
  select Line Whole
  press right
 Edit Select _ Character -> do -- special behavior
  mark
  press right
 Edit Select s r -> do
  select r s  -- generic behavior
  activate_mark
 -- some Regions need a { press right } for idempotency of their beg_of/end_of

 Edit Google s r -> do
  google =<< selected s r

 Edit Delete s r -> do
  select r s
  press del

 Edit Copy s r -> do
  select r s
  press M 'c'                     -- like Cua-mode for Mac

 Edit Cut s r -> do
  select r s
  press M 'x'                     -- like Cua-mode for Mac

 Edit Transpose _ Character -> press C 't'
 Edit Transpose _ Word_     -> press M 't'
 Edit Transpose _ Group     -> press C M 't'
 Edit Transpose _ Line      -> press C 'x' 't'
 Edit Transpose _ Block     -> runEmacs "transpose-block" -- nonstandard
 -- Edit Transpose _ ->

 -- That
 -- Character
 -- Word_
 -- Token
 -- Group
 -- Line
 -- Rectangle
 -- Block
 -- Page
 -- Screen
 -- Everything
 -- Definition
 -- Function_
 -- Reference
 -- Structure

 _ -> nothing


-- ================================================================ --

-- it seems to be synchronous, even with threaded I guess?
attemptAsynchronously :: Int -> IO () -> IO ()
attemptAsynchronously seconds action = do
 (timeout (seconds * round (1e6::Double)) action) `withAsync` (waitCatch >=> \case
   Left error     -> print error
   Right Nothing  -> putStrLn "..."
   Right (Just _) -> return ()
  )

attempt = attemptAsynchronously 1

attemptMunge :: String -> IO ()
attemptMunge s = do
 putStrLn ""
 putStrLn ""
 putStrLn ""
 print s
 attempt $ parseBest bestPhrase phrase_ ((T.words . T.pack) s) & \case
  Left e -> print e
  Right raw_p -> do
   let pasted_p   = pPhrase raw_p
   let splatted_p = splatPasted pasted_p ("clipboard contents")
   let munged_p   = mungePhrase splatted_p defSpacing
   ol [ show raw_p
      , show pasted_p
      , show splatted_p
      , munged_p
      ]

attemptMungeAll :: String -> IO ()
attemptMungeAll s = do
 putStrLn ""
 putStrLn ""
 putStrLn ""
 print s
 attempt $ parseThrow phrase_ ((T.words . T.pack) s) >>= \case
  (raw_p :| raw_ps) -> do
   let pasted_p   = pPhrase raw_p
   let splatted_p = splatPasted pasted_p ("clipboard contents")
   let munged_p   = mungePhrase splatted_p defSpacing
   ol [ show raw_p
      , List.intercalate "\n , " $ map show $ raw_ps -- generate lazily
      , show pasted_p
      , show splatted_p
      , show munged_p
      ]

-- pseudo HTML ordered list
ol xs = ifor_ xs $ \i x -> do
 putStrLn ""
 putStrLn $ fold [show i, ". ", x]

attemptParse :: (Show a) => (forall  z. DNSEarleyRHS z a) -> String -> IO ()
attemptParse rule s = do
 putStrLn ""
 attempt $ parseThrow rule ((T.words . T.pack) s) >>= \case
  x :| _ -> print x

attemptSerialize rhs = attemptAsynchronously 3 $ do
 serialized <- formatRHS rhs
 either print printSerializedGrammar serialized

printSerializedGrammar SerializedGrammar{..} = do
 replicateM_ 3 $ putStrLn ""
 T.putStrLn $ displayDoc serializedRules
 putStrLn ""
 T.putStrLn $ displayDoc serializedLists



realMain = do

 putStrLn ""
 let rootG = root
 attemptSerialize rootG

 attemptMunge "par round grave camel lit with async break break action"

main = do
 realMain

