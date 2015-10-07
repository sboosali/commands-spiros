{-# LANGUAGE GeneralizedNewtypeDeriving, AutoDeriveTypeable, DeriveDataTypeable, DeriveFunctor, TypeFamilies         #-}
module Commands.Plugins.Spiros.Phrase.Types where

import           Data.Sexp
import           Commands.Etc

import           Data.List.NonEmpty               (NonEmpty)

import           GHC.Exts                         (IsString (..),IsList (..))


-- ================================================================ --
-- "static" phrase

newtype Phrase = Phrase { unPhrase :: [Phrase_] } 
 deriving(Show,Eq,Ord,Data,Monoid)

instance IsString Phrase where
 fromString = word2phrase

{- | 

'Phrase' versus 'UPhrase':

@Phrase_@ is the unassociated concrete syntax list (e.g. tokens, parentheses),
while @Phrase@ is the associated abstract syntax tree (e.g. s-expressions).

-}
data Phrase_
 = Escaped_  Keyword -- ^ atom-like (wrt 'Sexp').
 | Quoted_   Dictation -- ^ list-like.
 | Pasted_ -- ^ atom-like.
 | Blank_ -- ^ atom-like.
 | Separated_ Separator -- ^ like a "close paren".
 | Bonked_              -- ^ like a "close paren". 
 | Cased_      Casing -- ^ function-like (/ "open paren").
 | Joined_     Joiner -- ^ function-like (/ "open paren").
 | Surrounded_ Brackets -- ^ function-like (/ "open paren").
 | Spelled_  [Char] -- ^ atom-like.
 | Capped_   [Char] -- ^ atom-like.
 | Dictated_ Dictation -- ^ list-like.
 deriving (Show,Eq,Ord,Data)

instance IsString Phrase_ where
 fromString = word2phrase_

data Casing = UpperCase | LowerCase | CapCase deriving (Show,Eq,Ord,Enum,Bounded,Data)

data Joiner = Joiner String | CamelJoiner | ClassJoiner | ShrinkJoiner deriving (Show,Eq,Ord,Data)

data Brackets = Brackets String String deriving (Show,Eq,Ord,Data)

newtype Separator = Separator String  deriving (Show,Eq,Ord,Data)

newtype Keyword = Keyword String  deriving (Show,Eq,Ord,Data)

newtype Dictation = Dictation [String] deriving (Show,Eq,Ord,Data)

instance IsString Dictation where
 fromString = Dictation . words
 -- safe: words "" == []

instance IsList Dictation where
 type Item Dictation = String
 fromList = Dictation
 toList (Dictation ws) = ws

-- no {JoinerFunction ([String] -> String)} to keep the {Eq} instance for caching
-- fake equality? {JoinerFunction Name ([String] -> String)} so {JoinerFunction 'camelCase camelCase}
-- maybe some type from data.split package, that both supports decidable equality and that can build functions




-- ================================================================ --
-- "dynamic" phrase

-- | "User-Facing Phrase". exists after (DSL-)parse-time.
type UPhrase  = Sexp PFunc (Either Pasted PAtom)
-- | "Mungeable Phrase". exists only at (DSL-)run-time.
-- the atom is really a list of atoms, but not a full Sexp. this supports "splatting". We interpret a list of atoms as string of words with white space between, more or less.
type MPhrase = Sexp PFunc [PAtom]

-- | a "Phrase Function".
data PFunc
 = Cased      Casing
 | Joined     Joiner
 | Surrounded Brackets
 deriving (Show,Eq,Ord)

-- | "Phrase Atom".
--
-- 'PAcronym's behave differently from 'PWord's under some 'Joiner's (e.g. class case).
-- 
data PAtom
 = PWord String
 | PAcronym Bool [Char]         -- ^ whether the acronym will be uppercased
 deriving (Show,Eq,Ord)

-- | for doctest
instance IsString PAtom where fromString = PWord


{- | a Pasted is like a 'Dictation'/'Quoted_' i.e. a list of words, not a single word.

we know (what words are in) the Dictation at (DSL-)"parse-time" i.e. 'phrase',
but we only know (what words are in) the Pasted at (DSL-)"runtime"
(i.e. wrt the DSL, not Haskell). Thus, it's a placeholder.

-}
data Pasted = Pasted  deriving (Show,Eq,Ord)

-- | used by 'pPhrase'.
type PStack = NonEmpty PItem

-- | an inlined subset of 'Sexp'.
--
-- Nothing represents 'List', Just represents 'Sexp', 'Atom' is not represented.
type PItem = (Maybe PFunc, [UPhrase])




-- ================================================================ --
-- helpers

-- unPhrase :: Phrase -> [Phrase_]
-- unPhrase (Phrase p) = p

asUPhrase :: String -> UPhrase
asUPhrase = Atom . Right . PWord

bracket :: Char -> Brackets
bracket c = Brackets [c] [c]

word2phrase_ :: String -> Phrase_
word2phrase_ = Dictated_ . Dictation . (:[])

word2phrase :: String -> Phrase
word2phrase = Phrase . (:[]) . word2phrase_

-- blankPhrase :: Phrase
-- blankPhrase = Phrase [Blank_]

-- snocPhrase :: Phrase -> String -> Phrase
-- snocPhrase p s = p ++ [fromString s]

-- mergeAdjacentDictated :: Phrase -> Phrase
-- mergeAdjacentDictated = id -- TODO

