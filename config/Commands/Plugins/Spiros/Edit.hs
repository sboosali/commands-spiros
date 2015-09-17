{-# LANGUAGE TemplateHaskell, RankNTypes, AutoDeriveTypeable, LambdaCase, PostfixOperators, PartialTypeSignatures, TupleSections  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-name-shadowing #-}  -- fewer type signatures (i.e. more type inference) makes the file more "config-like"
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Edit where 
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Emacs
import           Commands.Plugins.Spiros.Emacs.Config

import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Mixins.DNS13OSX9
import           Commands.Sugar.Alias
import           Commands.Sugar.Press

import           Control.Applicative


-- ================================================================ --

data Move
 = Move   Direction Region
 | MoveTo Endpoint  Region
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
data Direction = Up_ | Down_ | Left_ | Right_ | In_ | Out_  deriving (Show,Eq,Ord,Enum)
direction = tidyGrammar
-- direction = transformedGrammar (filter (/= '_'))
-- direction = qualifiedGrammarWith "_"

-- ================================================================ --

data Edit = Edit Action Slice Region deriving (Show,Eq,Ord)

edit = 'edit <=> empty 
 -- <|> Edit Cut Forwards Line <#> "kill"
 --     -- i.e. "kill" -> "kill for line", not "kill whole that"
 <|> Edit <$> action              <*> (slice -?- defaultSlice) <*> (region -?- defaultRegion)
    -- e.g. "cop" or "cop that" or "cop whole" -> "cop whole that"
 <|> Edit <$> (action -?- defaultAction) <*> slice             <*> region
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
 deriving (Show,Eq,Ord)

action = 'action <=> empty
 <|> Select      <#> "sell"
 <|> Copy        <#> "cop"
 <|> Cut         <#> "kill"      -- "cut" 
 <|> Delete      <#> "del"
 <|> Transpose   <#> "trans"
 <|> Google      <#> "google"

{- | slice the region between the cursor and the 'Slice'. induces a string.
-}
data Slice = Whole | Backwards | Forwards  deriving (Show,Eq,Ord,Enum)
slice = 'slice <=> vocab
 [ "whole"-:Whole
 , "back"-: Backwards
 , "fuss"-: Forwards
 ]
 -- "for" would be homophone with "four", while both Positive and Slice can be the prefix (i.e. competing for the same recognition).
-- "four kill for word" should be [4 Kill Forwards Word] not [4 Kill Forwards Line, 4 Sel Whole Word]
-- "four kill fuss word" is unambiguously [4 Kill Forwards Word]

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
 deriving (Show,Eq,Ord,Enum)

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

defaultAction = Select 
defaultSlice = Whole
defaultRegion = That

rankMove _m = 0

rankEdit (Edit a s r) = (sum . fmap fromEnum) [a /= defaultAction, s /= defaultSlice, r /= defaultRegion]
-- defaults have lower rank
-- disambiguates "cop fuss line" into [cop fuss line], not [cop] [fuss line] which comes first in the order 

runMove = moveEmacs

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

runEdit = editEmacs

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
