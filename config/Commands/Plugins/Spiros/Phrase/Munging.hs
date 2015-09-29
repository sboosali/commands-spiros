{-# LANGUAGE LambdaCase #-}
module Commands.Plugins.Spiros.Phrase.Munging where
import           Commands.Plugins.Spiros.Phrase.Types
import           Commands.Plugins.Spiros.Spacing

import qualified Commands.Backends.OSX            as OSX
import           Commands.Munging

import           Data.Sexp

import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty

import qualified Data.List               as List
import Data.Monoid                           ((<>))
import           Data.Foldable                    (Foldable (..))


-- | splats the Pasted into PAtom's, after splitting the clipboard into words
splatPasted :: UPhrase -> OSX.ClipboardText -> MPhrase
splatPasted p clipboard = either (substPasted clipboard) (:[]) <$> p
 where
 substPasted pasted Pasted = fmap PWord (words pasted)

mungePhrase :: MPhrase -> Spaced String
mungePhrase p = concatPAtoms =<< evalSplatSexp applyPFunc p

{- |


>>> :set -XOverloadedStrings
>>> concatPAtoms ["",""]
PWord ""
>>> concatPAtoms ["",""]
PWord ""

-}
concatPAtoms :: [PAtom] -> Spaced String
concatPAtoms xs e = (flip spaceOut) e . fmap mungePAtom $ xs

mungePAtom :: PAtom -> String
mungePAtom = \case
 PWord    x  -> x
 PAcronym False cs -> lower cs
 PAcronym True  cs -> upper cs

applyPFunc :: [PAtom] -> PFunc -> Spaced [PAtom]
applyPFunc as = \case
  Cased      g -> traverse (return . caseWith g) as
  Joined     g -> return [joinWith g as]
  Surrounded g -> surroundWith g as

caseWith :: Casing -> (PAtom -> PAtom)
caseWith c = mapPAtom (fromCasing c)

fromCasing :: Casing -> (String -> String)
fromCasing = \case
 UpperCase  -> upper
 LowerCase  -> lower
 CapCase    -> capitalize

mapPAtom :: (String -> String) -> (PAtom -> PAtom)
mapPAtom f = \case
 PWord    x  -> PWord    $ f x
 PAcronym b cs -> PAcronym b $ f cs
 -- PWord . f . mungePAtom

joinWith :: Joiner -> ([PAtom] -> PAtom)
joinWith = \case
 -- Joiner s    -> List.interleave (PWord s)
 Joiner s    -> PWord . List.intercalate s . fmap mungePAtom
 CamelJoiner -> PWord . camelAtoms
 ClassJoiner -> PWord . classAtoms

camelAtoms :: [PAtom] -> String
camelAtoms []     = ""
camelAtoms (x:xs) = lower (mungePAtom x) <> (classAtoms xs)

classAtoms :: [PAtom] -> String
classAtoms = squeezeCase . (fmap $ \case
 PWord w     -> capitalize w
 PAcronym _ cs -> upper cs)
-- TODO distinguish Capped from Acronym to preserve capitalization?

surroundWith :: Brackets -> ([PAtom] -> Spaced [PAtom])
surroundWith (Brackets l r) as = do
 -- xs <- traverse mungePAtom as
 return $ ([PWord l] <> as <> [PWord r])
-- TODO generalize by renaming surround to transform: it shares the type with Interleave
-- e.g. "par thread comma 123" -> (1,2,3)

joinSpelled :: [Phrase_] -> [Phrase_]
joinSpelled = foldr' go []
 where
 go :: Phrase_ -> [Phrase_] -> [Phrase_]
 go (Spelled_ xs) (Spelled_ ys : ps) = (Spelled_ $ xs <> ys) : ps
 go p ps = p:ps

-- | parses "tokens" into an "s-expression". a total function.
pPhrase :: [Phrase_] -> UPhrase
pPhrase = fromStack . foldl' go ((Nothing, []) :| []) . joinSpelled
 -- (PSexp (PList [PAtom (PWord "")]))
 where
 go :: PStack -> Phrase_ -> PStack
 go ps = \case
  (Escaped_  (x))            -> update ps $ fromPAtom (PWord x)
  (Quoted_   (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Dictated_ (Dictation xs)) -> update ps $ List ((fromPAtom . PWord) <$> xs)
  (Capped_   cs)             -> update ps $ fromPAtom (PAcronym True cs)
  (Spelled_  cs)             -> update ps $ fromPAtom (PAcronym False cs)
  Pasted_                    -> update ps $ fromPasted
  Blank_                     -> update ps $ fromPAtom (PWord "")
  Bonked_                    -> update (popall ps) $ fromPAtom (PWord " ")
  Separated_ (Separator x)   -> update (pop ps) $ fromPAtom (PWord x)
  -- Separated_ Broken -> update (pop ps)
  (Cased_     f)  -> push ps (Cased f)
  (Joined_    f)  -> push ps (Joined f)
  (Surrounded_ f) -> push ps (Surrounded f)

 popall :: PStack -> PStack
 -- breaks out of every func to the left (like having enough Separated_'s) 
 -- i.e. close the S expression with as many right parenthesis as it takes "...)))"
 popall (p:|(q:qs)) = popall (update (q:|qs) (fromItem p))
 -- can't pop any more
 popall stack = stack

 pop :: PStack -> PStack
 -- break from the innermost PFunc, it becomes an argument to the outer PFunc
 -- i.e. close the S expression with a right parenthesis "...)"
 pop (p:|(q:qs)) = update (q:|qs) (fromItem p)
 -- if too many breaks, just ignore
 pop stack = stack

 -- i.e. open a left parenthesis with some function "(f ..."
 push :: PStack -> PFunc -> PStack
 push (p:|ps) f = (Just f, []) :| (p:ps)

 update :: PStack -> UPhrase -> PStack
 update ((f,ps):|qs) p = (f, ps <> [p]) :| qs

 -- right-associate the PFunc's.
 fromStack :: PStack -> UPhrase
 fromStack = fromItem . foldr1 associateItem . NonEmpty.reverse

 associateItem :: PItem -> PItem -> PItem
 associateItem (f,ps) gqs = (f, ps <> [fromItem gqs])

 fromItem :: PItem -> UPhrase
 fromItem (Nothing, ps) = List   ps
 fromItem (Just f,  ps) = Sexp f ps

 fromPasted :: UPhrase
 fromPasted = (Atom . Left) Pasted

 fromPAtom :: PAtom -> UPhrase
 fromPAtom = Atom . Right

