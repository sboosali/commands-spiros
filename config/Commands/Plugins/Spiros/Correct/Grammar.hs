{-# LANGUAGE TemplateHaskell, OverloadedStrings, PostfixOperators, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Correct.Grammar  where 
-- import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Digit
import Commands.Plugins.Spiros.Correct(Correction) 
import Commands.Plugins.Spiros.Phrase

import Commands.Mixins.DNS13OSX9
import Control.Applicative


correctionGrammar :: R z Correction -- TODO, upon grammatical contexts  
correctionGrammar = 'correctionGrammar <=> empty
 <|> Left <$> digit_
 <|> (Right . letters2dictation) <$ "spell" <*> letters
 <|> Right <$> dictation 

runCorrection = either runDigit runDictation 
 where runDigit = insertD . digit2dictation 

digit2dictation :: Digit -> Dictation 
digit2dictation (Digit d) = words2dictation . show $ d 

