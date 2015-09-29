{-# LANGUAGE PostfixOperators, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Phrase
 ( module Commands.Plugins.Spiros.Phrase
 , module Commands.Plugins.Spiros.Phrase.Types
 , module Commands.Plugins.Spiros.Phrase.Run
 , module Commands.Plugins.Spiros.Phrase.Munging
 ) where
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Phrase.Types
import           Commands.Plugins.Spiros.Phrase.Run
import           Commands.Plugins.Spiros.Phrase.Munging
import           Commands.Plugins.Spiros.Spacing

import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import qualified Commands.Backends.OSX            as OSX

import qualified Data.Text.Lazy                   as T

import           Control.Applicative
import           Data.Char
import           Data.Foldable                    (Foldable (..))
import           Prelude                          hiding (foldr1, mapM)


phraseCommand :: DNSEarleyCommand z Phrase
phraseCommand = Command phrase bestPhrase $ \_ p -> do
 s <- OSX.getClipboard
 OSX.sendText (runPhrase_ defSpacing s p)

-- ================================================================ --

-- -- | transforms "token"s from 'phrase' into an "s-expression" with 'pPhrase'.
-- phrase = pPhrase <$> phrase

phrase :: DNSEarleyRHS z Phrase
phrase = Phrase <$> complexGrammar 'phrase
 -- NOTE phrase is still a NonTerminal, given instance Applicative RHS, because:
 -- either: (f <$> NonTerminal ...) is (fmap f (NonTerminal ...)) is (NonTerminal ... ) 
 -- or:     (f <$> NonTerminal ...) is (Pure f <*> NonTerminal ...) is (NonTerminal ... ) 

 -- (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 -- (conssnoc <$> (phraseA) <*> ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD))
 -- where
 -- conssnoc x ys z = [x] <> ys <> [z]

 (snoc     <$>             ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))
 (snoc     <$>             ((phraseA <|> phraseB <|> phraseD)-*) <*> (phraseB <|> phraseC <|> phraseD))

-- | a sub-phrase where a phrase to the right is certain.
--
-- this ordering prioritizes the escaping Escaped_/Quoted_ over the
-- escaped, e.g. "quote greater equal unquote".
phraseA :: DNSEarleyRHS z Phrase_
phraseA = 'phraseA <=> empty
 <|> Escaped_    <#> "lit" # keyword
 <|> Quoted_     <#> "quote" # dictation # "unquote"
 <|> Pasted_     <#> "pasted"    -- "yank" 
 <|> Blank_      <#> "blank"
 -- <|> Spelled_    <#> "spell" # (character-++)
 -- <|> Spelled_    <#> "lets" # letters -- (letter-++)
 <|> Bonked_     <#>  "smack"   -- (like saying "break" enough times) 
 <|> Separated_  <#> separator
 <|> Cased_      <#> casing
 <|> Joined_     <#> joiner
 <|> Surrounded_ <#> brackets

-- | a sub-phrase where a phrase to the right is possible.
phraseB :: DNSEarleyRHS z Phrase_
phraseB = 'phraseB <=> empty
 <|> Spelled_  <#> "let's" # (character-++)  -- abbreviation for "letters" 
 <|> Capped_   <#> "caps"  # (character-++)  -- abbreviation for "capital letters" 
 -- <|> Spelled_ <$> (phoneticAlphabetRHS-++)  
 <|> Pasted_     <#> "pasted"    -- "yank" 
 <|> Blank_      <#> "blank"
 -- TODO letters grammar that consumes tokens with multiple capital letters, as well as tokens with single aliases
 -- <|> Spelled_  <#> "spell" # letters -- only, not characters

-- | a sub-phrase where a phrase to the right is impossible.
phraseC :: DNSEarleyRHS z Phrase_
phraseC = 'phraseC <=> Dictated_ <#> "say" # dictation

-- | injects word_ into phrase
phraseW :: DNSEarleyRHS z Phrase_
phraseW = 'phraseW <=> word2phrase_ <#> word_

-- | injects dictation into phrase_
phraseD :: DNSEarleyRHS z Phrase_
phraseD = 'phraseD <=> Dictated_ <#> dictation

separator = 'separator <=> empty
 <|> Separator ""  <#> "break" --TODO separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
 <|> Separator " " <#> "space"
 <|> Separator "," <#> "comma"

casing = 'casing
 <=> LowerCase <$ token "lower"
 <|> UpperCase <$ token "upper"
 <|> CapCase   <$ token "copper"              -- "capper" 

joiner = 'joiner
 <=> (\c -> Joiner [c]) <#> "join" # character
 <|> Joiner "_" <#> "snake"
 <|> Joiner "-" <#> "dash"
 -- <|> Joiner "/" <#> "file"
 <|> Joiner ""  <#> "squeeze"
 <|> CamelJoiner <#> "camel"    -- "cam"
 <|> ClassJoiner <#> "class"

brackets = 'brackets
 <=> bracket          <#> "round" # character
 <|> Brackets "(" ")" <#> "par"
 <|> Brackets "[" "]" <#> "square"
 <|> Brackets "{" "}" <#> "curl"
 <|> Brackets "<" ">" <#> "angle"
 <|> bracket '"'      <#> "string"
 <|> bracket '\''     <#> "ticked"
 <|> bracket '|'      <#> "norm"
 -- <|> Brackets "**" "**" <#> "bold"

-- disjoint vocabulary ("effects"), possibly overlapping parses ("results")
character :: DNSEarleyRHS z Char
character = 'character <=> empty
 <|> punctuationRHS
 <|> englishNumericRHS
 <|> literalNumericRHS
 <|> phoneticAlphabetRHS
-- <|> shortAlphabetRHS
-- <|> literalAlphabetRHS

punctuationRHS :: DNSEarleyRHS z Char
punctuationRHS = vocab
 [ "grave"-: '`'
 , "till"-: '~'
 , "bang"-: '!'
 , "axe"-: '@'
 , "pound"-: '#'
 , "doll"-: '$'
 , "purse"-: '%'
 , "care"-: '^'
 , "amp"-: '&'
 , "star"-: '*'
 , "lore"-: '('
 , "roar"-: ')'
 , "hit"-: '-'                  -- during a Phrase,Dragon recognizes "dash" literally as "-" 
 , "score"-: '_'
 , "equal"-: '='
 , "plus"-: '+'
 , "lack"-: '['
 , "lace"-: '{'
 , "rack"-: ']'
 , "race"-: '}'
 , "stroke"-: '\\'
 , "pipe"-: '|'
 , "sim"-: ';'
 , "coal"-: ':'
 , "tick"-: '\''
 , "quote"-: '"'
 , "com"-: ','
 , "less"-: '<'
 , "dot"-: '.'
 , "great"-: '>'
 , "slash"-: '/'
 , "quest"-: '?'
 , "tab"-: '\t'
 , "ace"-: ' '
 , "ret"-: '\n'  -- "line" conflicts with (Line :: Region)  
 ]

englishNumericRHS :: DNSEarleyRHS z Char
englishNumericRHS = vocab
 [ "zero"-: '0'
 , "one"-: '1'
 , "two"-: '2'
 , "three"-: '3'
 , "four"-: '4'
 , "five"-: '5'
 , "six"-: '6'
 , "seven"-: '7'
 , "eight"-: '8'
 , "nine"-: '9'
 ]

-- | @('read' <$> digits :: R_ 'Int')@ is total.
digits = 'digits <=> (digit-++)

digit :: R z Char
digit = 'digit <=> (head . show) <$> digitRHS

digitRHS :: (Num a) => R z a
digitRHS = vocab
 [ "nil"   -: 0                  -- monosyllabic
 , "zero"  -: 0                 -- disyllabic
 , "one"   -: 1
 , "two"   -: 2
 , "three" -: 3
 , "four"  -: 4
 , "five"  -: 5
 , "six"   -: 6
 , "sev"   -: 7                  -- monosyllabic
 , "seven" -: 7                -- disyllabic
 , "eight" -: 8
 , "nine"  -: 9
 ]

number :: R z Number
number = 'number <=> numberRHS

numberRHS :: (Num a) => R z a
numberRHS = digitRHS <|> vocab
 [ "ten"-: 10
 , "eleven"-: 11
 , "twelve"-: 12
 , "thirteen"-: 13
 , "fourteen"-: 14
 , "fifteen"-: 15
 , "sixteen"-: 16
 , "seventeen"-: 17
 , "eighteen"-: 18
 , "nineteen"-: 19
 , "twenty"-: 20
 , "twenty-one"-: 21
 , "twenty-two"-: 22
 , "twenty-three"-: 23
 , "twenty-four"-: 24
 , "twenty-five"-: 25
 , "twenty-six"-: 26
 , "twenty-seven"-: 27
 , "twenty-eight"-: 28
 , "twenty-nine"-: 29
 , "thirty"-: 30
 , "thirty-one"-: 31
 , "thirty-two"-: 32
 , "thirty-three"-: 33
 , "thirty-four"-: 34
 , "thirty-five"-: 35
 , "thirty-six"-: 36
 , "thirty-seven"-: 37
 , "thirty-eight"-: 38
 , "thirty-nine"-: 39
 , "forty"-: 40
 , "forty-one"-: 41
 , "forty-two"-: 42
 , "forty-three"-: 43
 , "forty-four"-: 44
 , "forty-five"-: 45
 , "forty-six"-: 46
 , "forty-seven"-: 47
 , "forty-eight"-: 48
 , "forty-nine"-: 49
 , "fifty"-: 50
 , "fifty-one"-: 51
 , "fifty-two"-: 52
 , "fifty-three"-: 53
 , "fifty-four"-: 54
 , "fifty-five"-: 55
 , "fifty-six"-: 56
 , "fifty-seven"-: 57
 , "fifty-eight"-: 58
 , "fifty-nine"-: 59
 , "sixty"-: 60
 , "sixty-one"-: 61
 , "sixty-two"-: 62
 , "sixty-three"-: 63
 , "sixty-four"-: 64
 , "sixty-five"-: 65
 , "sixty-six"-: 66
 , "sixty-seven"-: 67
 , "sixty-eight"-: 68
 , "sixty-nine"-: 69
 , "seventy"-: 70
 , "seventy-one"-: 71
 , "seventy-two"-: 72
 , "seventy-three"-: 73
 , "seventy-four"-: 74
 , "seventy-five"-: 75
 , "seventy-six"-: 76
 , "seventy-seven"-: 77
 , "seventy-eight"-: 78
 , "seventy-nine"-: 79
 , "eighty"-: 80
 , "eighty-one"-: 81
 , "eighty-two"-: 82
 , "eighty-three"-: 83
 , "eighty-four"-: 84
 , "eighty-five"-: 85
 , "eighty-six"-: 86
 , "eighty-seven"-: 87
 , "eighty-eight"-: 88
 , "eighty-nine"-: 89
 , "ninety"-: 90
 , "ninety-one"-: 91
 , "ninety-two"-: 92
 , "ninety-three"-: 93
 , "ninety-four"-: 94
 , "ninety-five"-: 95
 , "ninety-six"-: 96
 , "ninety-seven"-: 97
 , "ninety-eight"-: 98
 , "ninety-nine"-: 99
 , "one-hundred"-: 100
 ]

{- | equivalent to:

@
 <|> '0' <$ "0"
 <|> '1' <$ "1"
 ...
 <|> '9' <$ "9"
@

-}
literalNumericRHS :: DNSEarleyRHS z Char
literalNumericRHS = foldMap (\c -> c <$ token [c]) ['0'..'9']

shortAlphabetRHS :: DNSEarleyRHS z Char
shortAlphabetRHS = vocab
 [ "ay"-: 'a'
 , "bee"-: 'b'
 , "sea"-: 'c'
 , "dee"-: 'd'
 , "eek"-: 'e'
 , "eff"-: 'f'
 , "gee"-: 'g'
 , "aych"-: 'h'
 , "eye"-: 'i'
 , "jay"-: 'j'
 , "kay"-: 'k'
 , "el"-: 'l'
 , "em"-: 'm'
 , "en"-: 'n'
 , "oh"-: 'o'
 , "pea"-: 'p'
 , "queue"-: 'q'
 , "are"-: 'r'
 , "ess"-: 's'
 , "tea"-: 't'
 , "you"-: 'u'
 , "vee"-: 'v'
 , "dub"-: 'w'
 , "ex"-: 'x'
 , "why"-: 'y'
 , "zee"-: 'z'
 ]

phoneticAlphabetRHS :: DNSEarleyRHS z Char
phoneticAlphabetRHS = vocab phoneticAlphabet

phoneticAlphabet :: [(String, Char)] 
phoneticAlphabet =
 [ "alpha"-: 'a'
 , "bravo"-: 'b'
 , "charlie"-: 'c'
 , "delta"-: 'd'
 , "echo"-: 'e'
 , "foxtrot"-: 'f'
 , "golf"-: 'g'
 , "hotel"-: 'h'
 , "india"-: 'i'
 , "juliet"-: 'j'
 , "kilo"-: 'k'
 , "lima"-: 'l'
 , "mike"-: 'm'
 , "november"-: 'n'
 , "oscar"-: 'o'
 , "poppa"-: 'p'
 , "quebec"-: 'q'
 , "romeo"-: 'r'
 , "sierra"-: 's'
 , "tango"-: 't'
 , "uniform"-: 'u'
 , "victor"-: 'v'
 , "whiskey"-: 'w'
 , "x-ray"-: 'x'
 , "yankee"-: 'y'
 , "zulu"-: 'z'
 ]

{- | equivalent to:

@
 <|> 'a' <#> "a"
 <|> 'b' <#> "b"
 <|> 'c' <#> "c"
 <|> ...
 <|> 'z' <#> "z"

 <|> 'a' <#> "A"
 <|> 'b' <#> "B"
 <|> 'c' <#> "C"
 <|> ...
 <|> 'z' <#> "Z"
@

-}
literalAlphabetRHS :: DNSEarleyRHS z Char
literalAlphabetRHS = foldMap (\c -> (c <$ token [c]) <|> (c <$ token [toUpper c])) ['a'..'z'] -- TODO What will we get back from Dragon anyway?

dictation = dragonGrammar 'dictation
 ((Dictation . fmap T.unpack) <$> some anyWord)
 (DGNDictation)
{-# NOINLINE dictation #-} --TODO doesn't help with the unshared <dictation__4>/<dictation__14>/<dictation__16>

word_ = dragonGrammar 'word_
 (T.unpack <$> anyWord)
 (DGNWords)

keyword :: R z String 
keyword = 'keyword
 <=> T.unpack <$> terminals

letters = simpleGrammar 'letters
 ((T.unpack . fold) <$> some anyWord)
 (DNSMultiple $ SomeDNSNonTerminal $ DNSBuiltinRule $ DGNLetters)

-- newtype Letters = Letters [Char] deriving (Show,Eq,Ord)
-- letters = (set dnsInline True defaultDNSInfo) $ 'letters <=>
--  Letters <#> (letter-+)







-- ================================================================ --

-- attemptMunge :: String -> IO ()
-- attemptMunge s = do
--  putStrLn ""
--  putStrLn ""
--  putStrLn ""
--  print s
--  attempt $ parseBest bestPhrase phrase ((T.words . T.pack) s) & \case
--   Left e -> print e
--   Right (Phrase raw_p)  -> do
--    let pasted_p   = pPhrase raw_p
--    let splatted_p = splatPasted pasted_p ("clipboard contents")
--    let munged_p   = mungePhrase splatted_p defSpacing
--    ol [ show raw_p
--       , show pasted_p
--       , show splatted_p
--       , munged_p
--       ]

-- attemptMungeAll :: String -> IO ()
-- attemptMungeAll s = do
--  putStrLn ""
--  putStrLn ""
--  putStrLn ""
--  print s
--  attempt $ parseThrow phrase ((T.words . T.pack) s) >>= \case
--   (Phrase raw_p :| raw_ps) -> do
--    let pasted_p   = pPhrase raw_p
--    let splatted_p = splatPasted pasted_p ("clipboard contents")
--    let munged_p   = mungePhrase splatted_p defSpacing
--    ol [ show raw_p
--       , List.intercalate "\n , " $ map show $ raw_ps -- generate lazily
--       , show pasted_p
--       , show splatted_p
--       , show munged_p
--       ]

