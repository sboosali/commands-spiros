{-# LANGUAGE RankNTypes, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, PartialTypeSignatures, DoAndIfThenElse #-}

{-|

Concept: Vocabulary list (Recognize one of) versus vocabulary buffer (Recognize any Contiguous subsequence)
A list can be a Production That's nested within Any grammar, But a buffer is its own grammar; This is a technical limitation, Rather than a necessary distinction.

A very simple shim, That must only be "installed" (write a file, or copy paste the contents into an editor)
Once (and whenever it changes, bbut not whenever the grammar changes).
all further communication Is networked, aand further  modifications  Can be bootstrapped.

Concept: the Phrase. An important Production That enables  Dictating complexly-formatted sentences/expressions Via a single utterance.
It interleaves arbitrary dictation with keywords. Examples of formatting include: Camel casing, capitalization, spacing, etc.


in Atom, or paredit mode in Emacs, only insert the left grouping character (e.g. "("),
since the right grouping character (e.g. ")") is inserted automatically and the cursor moved back

TODO the sounds of typing on the keyboard is recognized as "up".

-}
module Commands.Spiros.Server where
import Commands.Plugins.Spiros hiding (mainWith)

import Commands.Servers.Simple
-- import Workflow.<platform>
import Commands.Backends.Workflow as W

import Commands.Frontends.Dragon13 as DNS
--import qualified Commands.Frontends.Dragon13 as DNS
import           Commands.Parsers.Earley (EarleyParser(..), bestParse, eachParse)
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd)
import  Workflow.Core         as W

import           Control.Lens
import           Data.Text.Lazy                        (Text)
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import Data.Char
import qualified Data.List as List
import           Control.Monad
import System.IO

import Prelude.Spiros
import Prelude()

{-|

If you get this error:

@
URLError: <urlopen error [Errno 10061] No connection could be made because the target machine actively refused it>

or

URLError: <urlopen error [Errno 10060] A connection attempt failed because the connected party did not properly respond after a period of time, or established connection failed because connected host has failed to respond>
@

You can fix it by:

Googling "Internet Explorer proxy settings" to find http://www.lib.berkeley.edu/using-the-libraries/proxy-ie10-11-windows

127.0.0.0 v 127.0.0.1 v localhost

TODO Document  fixing this connection refusal   error In more places

the python documentation for urllib.urlopen:
"""
In a Windows environment, if no proxy environment variables are set,
proxy settings are obtained from the registry's Internet Settings
section.
"""

ipconfig /flushdns And restart ??

Open Start Menu and click Control Panel
Click Security (or if you are in classic view click Windows Firewall and goto step 4)
Click 'Allow a program through windows firewall'
Click Change Settings
Click the Exceptions tab
Ensure there is a check aside World Wide Web Services (HTTP)

127.0.0.0 times out, 127.0.0.1 Is actively refused, and localhost Is not found??
but http://127.0.0.1:8888/test still works from browser

whitelist localhost

Fix: (A simpler or automated solution probably exists)
edit the Windows firewall Settings to white list localhost connections from the current user.
E.g.


-}
main = do
  --
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering

  mainWith myEnvironment

myEnvironment :: _
myEnvironment = VEnvironment myPlugin myNatlink

myPlugin :: _
myPlugin = spirosUpdate
  (defaultDnsOptimizationSettings & dnsOptimizeSerialization.serializationForceWordWrap .~ True)
	rootCommand

myNatlink = NatLinkSettings{..} -- "" -- Todo remove context  fields From NatLinkConfig
  where
  nlAddress = Address (Host "127.0.0.1") (Port 8888) -- Todo Write-once The simple-server's settings&Port
  nlLocation = "C:/NatLink/NatLink/MacroSystem/_commands.py"
  -- inDirectory = ("C:/NatLink/NatLink/MacroSystem/"++)

data NatLinkSettings = NatLinkSettings
 { nlAddress :: Address
 , nlLocation :: FilePath  -- todo Add a field to Config
 }

data Invoker m a = Invoker
  { parse   :: [Text] -> Maybe a
  , compile :: a -> m ()
  }

data Invocation m a = Invocation
 { recognized :: [Text]
 , parsed     :: Maybe a
 , compiled   :: m ()
-- , executed   :: Bool
 }

{-| the fields in the output are constructed lazily,
with intermediary computations shared.

-}
-- handleRequest :: CommandsHandlers a b -> CommandsRequest -> CommandsResponse a b
invoke :: (Applicative m) => Invoker m a -> [Text] -> Invocation m a
invoke Invoker{..} ws = Invocation{..}
 where
 recognized = ws
 parsed     = recognized & parse
 compiled   = parsed & maybe (pure()) compile

{- |

@

$ stack build && stack exec -- commands-spiros-server
> camel two words
Right (Phrase_ (Phrase [Joined_ CamelJoiner,Dictated_ (Dictation ["two","words"])]))
...

@

'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'

-}
mainWith environment@VEnvironment{..} = do
  --	putStrLn $ displaySerializedGrammar (ePlugin&vGrammar)
  putStrLn "(commands-spiros-server...)"
  spirosSetup environment
  runSimpleServer _settings

  where
  _settings = (defaultSettings defaultWindowsExecuteWorkflow) -- TODO This is  platform specific
    { handle = myHandle ePlugin
    , cmdln = Nothing -- Just $ myCmdln ePlugin
    }

myCmdln ePlugin (words -> fmap T.pack -> ws) = do
	print $ bestParse (ePlugin&vParser) ws
	--print =<< W.currentApplication
	putStrLn""

-- myHandle = defaultHandle
myHandle ePlugin recognition = do
  let ws = recognition & fmap T.pack
  liftIO$ do
      putStrLn "----------------------------------------------------------------------------------"
      putStrLn$ "Recognition:"
      print $ recognition
      putStrLn ""

  -- context <- W.currentApplication
  let context = GlobalContext -- ""

  if (isNoise recognition) -- TODO a "results type" for the parsing stage , either failure, success, or noise (and later maybe  other cases)
  then liftIO$ do
    putStrLn$ "NOISE:"
    print $ myNoise
    putStrLn$ "WORDS:"
    putStrLn$ showWords ws
  else liftIO$ do
    go context ws

 where
 go context ws = do
  case bestParse (ePlugin&vParser) ws of
    Right x -> do
      -- if (isNoise (ws & fmap T.unpack)) -- TODO a "results type" for the parsing stage , either failure, success, or noise (and later maybe  other cases)
        _display context x
        exec context x

    Left e -> do
    	liftIO$ do
    	   replicateM_ 3 (putStrLn"")
           putStrLn$ "ERROR:"
           print e
           putStrLn$ "WORDS:"
           putStrLn$ showWords ws
           hFlush stdout

  where
  _display context value = liftIO$ do
      putStrLn$ "VALUE:"
      print value
      putStrLn ""
      putStrLn$ "CONTEXT:"
      print context
      putStrLn ""
      putStrLn$ "WORDS:"
      putStrLn$ showWords ws
      putStrLn ""

  exec context value = liftIO$ do
    runSpirosMonad $ (ePlugin&vDesugar) context value

runSpirosMonad = getSpirosMonad > W.runWorkflowWithT def{windowsCharacterDelay=0} --TODO

myNoise = ["the","will","if","him","that","a","she","and","up","noise" ] & fmap (:[])
isNoise ws = (ws `elem` myNoise)

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

--------------------------------------------------------------------------------

data VEnvironment m c v = VEnvironment
 { ePlugin :: VPlugin m c v
 , eSettings :: NatLinkSettings
 }

spirosSetup
 :: _
 -> IO (Either String ())
-- spirosSetup VEnvironment{ePlugin{..},eConfig{..}} = do
spirosSetup environment = do
 let theGrammar = (environment&ePlugin&vGrammar)
 let theSettings@NatLinkSettings{..} = environment&eSettings -- TODO theAddress
 let theConfig = DNS.NatLinkConfig nlAddress -- TODO LOL get rid of  config

 do
   putStrLn ""
   T.putStrLn$ displayAddress nlAddress
   -- T.putStrLn$ curl_ExampleRequest nlAddress

 -- do
 --   let myBatchScript = getBatchScript myBatchScriptR
 --
 --   putStrLn ""
 --   T.putStrLn$ myBatchScript
 --   setClipboardIO$ T.unpack myBatchScript
 --
 --   putStrLn ""
 --   putStrLn$ getBatchScriptPath myBatchScriptR
 --
 --   setClipboardIO$ T.unpack (myBatchScriptR&__batchFilePath__)

 let theShim = fmap (over _PythonFile _cleanShim) $ DNS.applyShim DNS.getShim theConfig theGrammar  -- TODO is this the right place?

 case theShim of

  Left (PythonSyntaxError e s) -> do
   let (errorRow, errorColumn) = getPythonErrorSpan e
   let (marginWidth, countWidth, code) = leftAppendLineNumbers s

   putStrLn ""
   print e
   putStrLn ""
   T.putStrLn$ code
   putStrLn ""
   -- W.runWorkflowT def $ findErrorBySearch countWidth marginWidth errorRow errorColumn
   putStrLn $ (show errorRow)<>":"<> (show errorColumn)
   putStrLn "SHIM PARSING FAILURE" -- TODO logging
   return$ Left("")

  Right myShim -> do

   T.putStrLn $ "writing shim to '"<>T.pack nlLocation<>"'"
   T.writeFile nlLocation (myShim&getPythonFile) --- Todo?

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

  --  -- because:
  --  -- 1. when pasting into an editor in virtual box, the clipboard contents are often trailed by Unicode garbage
  --  -- 2. which is why the shim ends in a comment
  --  -- 3. but Unicode characters can have nonlocal effects on other characters, like on previous lines
  --  copyShim myShim
  --  writeShim myBatchScriptR myShim -- TODO its own function

   putStrLn "SHIM PARSING SUCCESS" -- TODO logging

   return$ Right()

  where
  _cleanShim :: Text -> Text
  _cleanShim = T.filter isAscii
