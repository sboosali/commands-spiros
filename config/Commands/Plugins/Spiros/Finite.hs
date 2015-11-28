{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, ExistentialQuantification, ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Finite where 
import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Edit 
import Commands.Plugins.Spiros.Root (click) 

import           Commands.Mixins.DNS13OSX9

import qualified Data.Text.Lazy as T


printClick = do
 
 let (gClick, pClick) = test_observeParserAndGrammar click
 putStrLn""
 print$ pClick "click"
 print$ pClick "double click"
 print$ pClick "middle click"
 print$ pClick "triple right click"
 print$ pClick "quadruple outer click"
 putStrLn""
 putStrLn$ gClick

printFinite = do
 putStrLn""
 putStrLn"FINITE"
 putStrLn""
 traverse_ printLength myFiniteSentences
 putStrLn""
 printAllSentences myFiniteSentences

 where
 printLength sentence = do 
  putStrLn $ "length = " ++ show (fmap length sentence)

printAllSentences someSentences  = do
 (traverse_.traverse_)  printSentences someSentences 

 where 
 printSentences sentences = do
  putStrLn""
  traverse_ printSentence $ sentences 

myFiniteSentences = theFiniteGrammars

 where 
 theFiniteGrammars
  = (fmap.fmap.fmap.fmap) T.unpack
  . fmap (\(SomeDNSEarleyRHS r) -> rhsEnumerateSentencesSimply r) 
  $ allFiniteRhs
--TODO (getRhsName r, isFiniteDNSEarleyGrammar r)

allFiniteRhs :: [SomeDNSEarleyRHS] 
allFiniteRhs =
 [ SomeDNSEarleyRHS move 
 , SomeDNSEarleyRHS edit 
 -- , SomeDNSEarleyRHS 
 ] 

