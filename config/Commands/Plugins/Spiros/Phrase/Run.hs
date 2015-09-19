{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-orphans #-} -- TODO orphans? 
module Commands.Plugins.Spiros.Phrase.Run where
import Commands.Plugins.Spiros.Phrase.Types
import Commands.Plugins.Spiros.Phrase.Munging
import           Commands.Plugins.Spiros.Etc

import qualified Commands.Backends.OSX            as OSX
import           Commands.Etc
import           Commands.Plugins.Example.Spacing
import Commands.Sugar.Press
import Commands.Sugar.Alias

import           Data.List.NonEmpty               (NonEmpty (..))

import           Control.Monad ((>=>)) 


runPhrase_ :: Spacing -> OSX.ClipboardText -> [Phrase_] -> String
runPhrase_ spacing clipboard
 = flip(mungePhrase) spacing
 . flip(splatPasted) clipboard
 . pPhrase

bestPhrase :: NonEmpty [Phrase_] -> [Phrase_]
bestPhrase = argmax rankPhrase

-- no list-generic instance, provides flexibility without OverlappingInstances  
instance Rankable Phrase' where rank = rankPhrase
instance Rankable (Maybe Phrase') where rank = maybe defaultRank rankPhrase

instance Rankable Phrase_ where rank = rankPhrase_ 
instance Rankable Dictation where rank = rankDictation 

rankPhrase :: Phrase' -> Int
rankPhrase = sum . fmap rankPhrase_

-- the specificity ("probability") of the phrase parts. bigger is better.
rankPhrase_ :: Phrase_ -> Int
rankPhrase_ = \case
 Escaped_ _    -> highRank
 Quoted_ _     -> highRank
 Pasted_       -> defaultRank
 Blank_        -> defaultRank
 Spelled_ cs    -> highRank + defaultRank * rankChars cs
 Capped_ cs     -> highRank + defaultRank * rankChars cs
 Separated_ _  -> defaultRank
 Cased_ _      -> defaultRank
 Joined_ _     -> defaultRank
 Surrounded_ _ -> defaultRank
 Dictated_ d   -> rankDictation d

rankChars cs = length cs - 1

rankDictation (Dictation ws) = length ws - 1
-- [Dictated_ ["some","words"]] =1 is better than [Dictated_ ["some"], Dictated_ ["words"]] =0

-- -- | convenience function for testing how phrase_ parses
-- parsePhrase_ :: [Text] -> String
-- parsePhrase_
--  = runPhrase_ defSpacing "clipboard contents"
--  . argmax rankPhrase
--  . NonEmpty.fromList --  TODO
--  . parseList phrase_

slotP :: Phrase' -> OSX.Actions_
slotP p' = do
 OSX.delay 10
 insertP p'
 press ret

insertP :: Phrase' -> OSX.Actions_
insertP = munge >=> OSX.insert

munge :: Phrase' -> OSX.Actions String
munge p1 = do
 p2 <- splatPasted (pPhrase p1) <$> OSX.getClipboard
 return$ mungePhrase p2 defSpacing

