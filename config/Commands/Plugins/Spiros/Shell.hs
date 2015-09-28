{-# LANGUAGE TemplateHaskell, PostfixOperators, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Shell where 
import           Commands.Plugins.Spiros.Phrase
import           Commands.Plugins.Spiros.Etc

import           Commands.Etc
import           Commands.Mixins.DNS13OSX9

import Data.Monoid                           ((<>))
-- import           Control.Applicative

data Safety
 = Safe     -- ^ shell commands that have no side effects, e.g. "ls"
            -- (it writes to the history, but that's a benign side effect). 
            --   or can be easily undone, e.g. "git stash". 
 | Unsafe   -- ^ e.g. "rm"  
 deriving (Show,Eq,Ord,Bounded,Enum)

data Shell                     
 = Shell Safety String Phrase -- TODO "safety" depends on options/subcommands, not just the command itself 
 deriving (Show,Eq,Ord)

shell = 'shell <=> foldMap go shellCommands
 where
 shellCommands =  fmap (leftAppend Safe)   (filterBlanks safeShellCommands)
               ++ fmap (leftAppend Unsafe) (filterBlanks unsafeShellCommands)
 go (safety,spoken,written) = Shell safety <$> (written <$ token spoken) <*> (phrase-?-"")
 leftAppend a (b,c) = (a,b,c) 

safeShellCommands = 
 [ "list"-: "ls"
 , "make dear"-: "mkdir"
 , "get"-: "git"
 , "CD"-: "cd"
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
 , ""-: ""
 , ""-: ""

 , both "cabal"
 , both "git"
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""

 ] 

unsafeShellCommands =
 [ "remove"-: "rm"
 , "remove dear"-: "rmdir"
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
 , ""-: ""
 , ""-: ""

 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""
 , both ""

 ]


-- ================================================================ --

instance (Rankable Shell) where rank = rankShell

rankShell :: Ranking Shell
rankShell = \case
 Shell Safe _cmd args -> rankPhrase args
 Shell Unsafe _cmd args -> rankPhrase args

runShell :: Desugaring Shell
runShell = \case

 Shell Safe cmd args -> do
  slotP$ word2phrase cmd <> " " <> args

 Shell Unsafe cmd args -> do
  insertP$ word2phrase cmd <> " " <> args

