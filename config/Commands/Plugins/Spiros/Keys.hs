{-# LANGUAGE LambdaCase, OverloadedStrings, PostfixOperators, RankNTypes #-}
{-# LANGUAGE TemplateHaskell                                #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Keys where
import Commands.Plugins.Spiros.Types
import           Commands.Plugins.Spiros.Phrase (character)

import           Commands.Backends.OSX
import           Commands.Etc
import           Commands.Mixins.DNS13OSX9

import           Control.Applicative


keyriff :: R z KeyRiff
keyriff = 'keyriff
 <=> (keychord-++)

-- | the terminals in key and modifier should be disjoint; otherwise, there is ambiguity.
keychord :: R z KeyChord
keychord = 'keychord
 <=> moveShift <$ "press" <*> (modifier-*)  <*> key      -- zero or more modifiers (prefixed) 
 <|> moveShift <$>            (modifier-++) <*> key      -- one or more modifiers (not prefixed) 
 where
 moveShift ms (ms', k) = KeyPress (ms ++ ms') k

modifier = 'modifier
 <=> "met"   $> CommandMod
 <|> "con"   $> Control
 <|> "shift" $> Shift
 <|> "alt"   $> Option
 <|> "fun"   $> Function

{- | a non-modifier key

'Key's and 'Char'acters are "incomparable sets":

* many modifiers are keys that aren't characters (e.g. 'CommandKey')
* many nonprintable characters are not keys (e.g. @\'\\0\'@)

so we can't embed the one into the other, but we'll just keep things simple with duplication.

-}
key :: R z KeyPress
key = 'key
 <=> ((either __BUG__ id) . char2keypress) <$> (__inlineRHS__(character))
  -- inlined to trigger vocabulary optimization, the right-hand side which must have only tokens

 <|> "up" $> KeyPress [] UpArrowKey
 <|> "down" $> KeyPress [] DownArrowKey
 <|> "left" $> KeyPress [] LeftArrowKey
 <|> "right" $> KeyPress [] RightArrowKey

 <|> "del" $> KeyPress [] DeleteKey
 <|> "cape" $> KeyPress [] EscapeKey
--  <|> functionKey
-- functionKey = empty
 <|> "F1" $> KeyPress [] F1Key
 <|> "F2" $> KeyPress [] F2Key
 <|> "F3" $> KeyPress [] F3Key
 <|> "F4" $> KeyPress [] F4Key
 <|> "F5" $> KeyPress [] F5Key
 <|> "F6" $> KeyPress [] F6Key
 <|> "F7" $> KeyPress [] F7Key
 <|> "F8" $> KeyPress [] F8Key
 <|> "F9" $> KeyPress [] F9Key
 <|> "F10" $> KeyPress [] F10Key
 <|> "F11" $> KeyPress [] F11Key
 <|> "F12" $> KeyPress [] F12Key
 <|> "F13" $> KeyPress [] F13Key
 <|> "F14" $> KeyPress [] F14Key
 <|> "F15" $> KeyPress [] F15Key
 <|> "F16" $> KeyPress [] F16Key
 <|> "F17" $> KeyPress [] F17Key
 <|> "F18" $> KeyPress [] F18Key
 <|> "F19" $> KeyPress [] F19Key
 <|> "F20" $> KeyPress [] F20Key

-- -- functionKey = empty
--  <|> "eff one" $> KeyPress [] F1Key  -- TODO can DNS vocabularies handle strings with multiple tokens? YES 
--  <|> "eff two" $> KeyPress [] F2Key
--  <|> "eff three" $> KeyPress [] F3Key
--  <|> "eff four" $> KeyPress [] F4Key
--  <|> "eff five" $> KeyPress [] F5Key
--  <|> "eff six" $> KeyPress [] F6Key
--  <|> "eff seven" $> KeyPress [] F7Key
--  <|> "eff eight" $> KeyPress [] F8Key
--  <|> "eff nine" $> KeyPress [] F9Key
--  <|> "eff ten" $> KeyPress [] F10Key
--  <|> "eff eleven" $> KeyPress [] F11Key
--  <|> "eff twelve" $> KeyPress [] F12Key
--  <|> "eff thirteen" $> KeyPress [] F13Key
--  <|> "eff fourteen" $> KeyPress [] F14Key
--  <|> "eff fifteen" $> KeyPress [] F15Key
--  <|> "eff sixteen" $> KeyPress [] F16Key
--  <|> "eff seventeen" $> KeyPress [] F17Key
--  <|> "eff eighteen" $> KeyPress [] F18Key
--  <|> "eff nineteen" $> KeyPress [] F19Key
--  <|> "eff twenty" $> KeyPress [] F20Key

-- | an ordinal numeral
ordinal :: R z Integer
ordinal = 'ordinal
 <=> unOrdinal <$> (__inlineRHS__(ordinalDigit))
 <|> vocab
  [ "tenth"-: 10
  , "eleven"-: 11
  , "twelve"-: 12
  , "thirteen"-: 13
  , "fourteen"-: 14
  , "fifteen"-: 15
  , "sixteen"-: 16
  , "seventeen"-: 17
  , "eighteen"-: 18
  , "nineteenth"-: 19
  , "twentieth"-: 20
  ]

-- | an ordinal numeral, between 0 (zeroth) and 9 (ninth) inclusive
ordinalDigit :: R z Ordinal
ordinalDigit = 'ordinalDigit <=> Ordinal <$> vocab
 [ "zeroth"-: 0
 , "first"-: 1
 , "second"-: 2
 , "third"-: 3
 , "fourth"-: 4
 , "fifth"-: 5
 , "sixth"-: 6
 , "seventh"-: 7
 , "eighth"-: 8
 , "ninth"-: 9
 ]

-- Keypress [Meta] CKey
--  OR
-- kbd "M-c"

