{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns, NamedFieldPuns, PartialTypeSignatures #-}

module Commands.Spiros.Server where
import Commands.Plugins.Spiros

import Commands.Servers.Simple
-- import Workflow.<platform>
import Commands.Backends.Workflow

import Commands.Frontends.Dragon13 as DNS
--import qualified Commands.Frontends.Dragon13 as DNS
import           Commands.Parsers.Earley (EarleyParser(..), bestParse, eachParse)
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd)
import  Workflow.Core         as W

import           Data.Text.Lazy                        (Text)
import qualified Data.Text.Lazy                as T
import Data.Char
import qualified Data.List as List
import           Control.Monad
import System.IO

import Prelude.Spiros
import Prelude()

{-

'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'

-}

main = do
	putStrLn "(commands-spiros-server)"
	runSimpleServer $ _settings

_settings = (defaultSettings defaultWindowsExecuteWorkflow)
 { handle = myHandle myPlugin 
 , cmdln = myCmdln myPlugin 
 }

myPlugin :: _
myPlugin = spirosUpdate defaultDnsOptimizationSettings rootCommand

myCmdln ePlugin (words -> fmap T.pack -> ws) = do
	print $ bestParse (ePlugin&vParser) ws
	--print =<< W.currentApplication
	putStrLn""

-- myHandle = defaultHandle
myHandle ePlugin (fmap T.pack -> ws) = do

  context <- W.currentApplication

  case bestParse (ePlugin&vParser) ws of
  -- {force} turns WHNF (the bang pattern) into NF
    Right x -> go context x
    Left e -> do
    	liftIO$ do
    	   replicateM_ 3 (putStrLn"")
           putStrLn$ "ERROR:"
           print e
           putStrLn$ "WORDS:"
           putStrLn$ showWords ws
           hFlush stdout

  where
  go context value = liftIO$ do
     putStrLn ""
     putStrLn$ "CONTEXT:"
     print context
     putStrLn ""
     putStrLn$ "VALUE:"
     print value
     putStrLn ""
     putStrLn$ "WORDS:"
     putStrLn$ showWords ws

----------------------------------------------------------------------------------

spirosUpdate
 :: DnsOptimizationSettings
 -- -> RULED ((Server.VSettings m c a)) r a
 -> (CMD c (m()) a)
 -> (VPlugin m c a)
spirosUpdate dnsSettings command = VPlugin g p d
 where
 g = (unsafeDNSGrammar dnsSettings (command&_cRHS))
 p = (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
 d = (command&_cDesugar)
{-# NOINLINE spirosUpdate #-}

type CMD c = DNSEarleyCommand c --TODO

{- | read-only.

"dynamic" configuration

-}
data VPlugin m c v = VPlugin
 { vGrammar :: DNS.SerializedGrammar
 , vParser  :: (forall s r. EarleyParser s r String Text v)
 , vDesugar :: c -> v -> m ()
 }

defaultHandle = handle
 where
 handle ws = if (ignore ws) then nothing else W.sendText (munge ws)
 munge = unwords > fmap toLower > (++ " ")
 ignore = unwords > (`elem` noise)
 noise = ["the","will","if","him","that","a","she","and"]

--
--TODO
--command2earley :: CMD c b a -> EarleyParser s r String Text a
--command2earley Command{..} = EarleyParser (unsafeEarleyProd _cRHS) _cBest

printMessage :: [String] -> IO ()
printMessage = putStrLn . List.intercalate "\n"

