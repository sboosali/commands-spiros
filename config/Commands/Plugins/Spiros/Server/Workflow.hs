{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Server.Workflow where 
import           Commands.Plugins.Spiros.Extra 
import           Commands.Plugins.Spiros.Edit (Move(..), Direction(..), Region(..), moveEmacs)
import           Commands.Plugins.Spiros.Phrase (word2phrase) 
import           Commands.Plugins.Spiros.Macros (reverse_search_regexp)  

import qualified Commands.Backends.OSX         as OSX

import Data.Monoid 


printWorkflow :: OSX.Workflow_ -> IO ()
printWorkflow = putStrLn . OSX.showWorkflow

insertByClipboardIO :: String -> IO ()
insertByClipboardIO s = OSX.runWorkflow $ do
 insertByClipboard ("\n" <> s <> "\n")
 OSX.delay 250 

printAndPaste :: String -> IO ()
printAndPaste s = do 
 insertByClipboardIO s 
 putStrLn s

{-| find a padded line number (to reach the one-based error column),
then move right (to the one-based error row).

-}
-- findErrorBySearch :: (OSX.MonadWorkflow m) => Int -> Int -> Int -> Int -> m () 
findErrorBySearch countWidth marginWidth errorRow errorColumn = do
 reverse_search_regexp (Just (word2phrase (padNumber countWidth errorRow))) 
 moveEmacs (Move Left_ Line)
 traverse_ moveEmacs (replicate (marginWidth + errorColumn - 2) (Move Right_ Character))

