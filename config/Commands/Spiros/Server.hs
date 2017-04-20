{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns, PartialTypeSignatures #-}

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
import Data.Char

import Prelude.Spiros
import Prelude()

{-

'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'

-}

main = do
	putStrLn "(commands-spiros-server)"
	runSimpleServer $ _settings

_settings = (defaultSettings defaultWindowsExecuteWorkflow) {
	handle = todo
}

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

