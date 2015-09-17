{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables                                          #-}
module Commands.Plugins.Spiros where
import           Commands.Plugins.Spiros.Root
import           Commands.Plugins.Spiros.Shim (getShim)

import qualified Commands.Backends.OSX         as OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13 hiding (getShim)
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant

import           Control.Lens                  hiding (from, ( # ))
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant
import Data.Time
import Data.Char

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe
import System.IO
import System.Mem

-- ================================================================ --

spirosServer :: IO ()
spirosServer = serveNatlink (spirosSettings rootsCommand)
-- spirosServer = spirosServe rootCommand
-- spirosServer = spirosServe rootPlugin

-- rootPlugin :: VPlugin_ r Root
-- rootPlugin = VPlugin rootCommand

-- de'serve :: (Show a) => (VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r a) a’ with ‘forall r1. VSettings_ r1 a0’
-- de'serve :: (Show a) => (forall r. VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r0 a) a’ with ‘forall r. VSettings_ r a0’
-- de'serve plugin = de'Settings plugin >>= serveNatlink

-- spirosServe :: (Show a) => (forall r. RULED DNSEarleyCommand r a) -> IO ()
-- -- de'serve plugin = unsafePerformIO(de'Settings plugin) & serveNatlink
-- spirosServe plugin = serveNatlink (spirosSettings plugin)

spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
spirosSettings command = VSettings 8888 spirosSetup spirosInterpret (spirosUpdateConfig command)

-- spirosSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- spirosSettings plugin = (defSettings runActions spirosUpdateConfig plugin)
--   { vSetup = spirosSetup
--   }

-- spirosSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- spirosSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runActions spirosUpdateConfig plugin)
--  return$ settings
--   { vSetup = setupCopyGrammar :: (VSettings (E.Rule r Root) Root -> IO (Either VError ()))
--   }

-- spirosUpdateConfig :: VPlugin (E.Rule r Root) Root -> IO (VConfig (E.Rule r Root) Root)
spirosUpdateConfig :: RULED DNSEarleyCommand r a -> RULED VConfig r a
spirosUpdateConfig command = unsafePerformIO$ do
 vGrammar <- de'deriveGrammarObservedSharing (command&_cRHS)
 -- let eProd = runST$ de'deriveParserObservedSharing (command&_cRHS)
 eProd <- unsafeSTToIO$ de'deriveParserObservedSharing (command&_cRHS) --TODO runST, but input is not rank2
 let vParser = EarleyParser eProd (command&_cBest)
 let vDesugar = (command&_cDesugar)
 return VConfig{..}
{-# NOINLINE spirosUpdateConfig #-}

spirosSetup :: RULED VSettings r a -> IO (Either VError ())
spirosSetup settings = do
 hSetBuffering stdout LineBuffering  -- a parser failure would exit in (EitherT IO), not printing the tokens or the error message

 let address = Address (Host "192.168.56.1") (Port (settings&vPort))

 applyShim getShim address (settings&vConfig&vGrammar) & \case
  Left e -> do
   putStrLn ""
   putStrLn$ (show e)
   return$ Left(VError (show e))

  Right (PythonFile shim) -> do
   putStrLn "" -- TODO logging

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

   OSX.runActions$ OSX.setClipboard (T.unpack (T.filter isAscii shim))

   T.putStrLn$ displayAddress address
   putStrLn ""

   return$ Right()

{- | this handler:

* supports short-circuiting (in 'EitherT'), returning an HTTP error status.
* executes the compiled actions (in 'IO').


-}
spirosInterpret :: (Show a) => (forall r. RULED VSettings r a) -> [Text] -> Response ()
spirosInterpret vSettings = \ws -> do

 t0<- liftIO$ getCurrentTime
 value <- e'ParseBest (vSettings&vConfig&vParser) ws & \case
  Right x -> return x
  Left e -> do
   liftIO$ do
    replicateM_ 3 (putStrLn"")
    putStrLn$ "ERROR:"
    print e
    putStrLn$ "WORDS:"
    T.putStrLn$ T.intercalate (T.pack " ") ws
    hFlush stdout
   left$ err400{errBody = BSC.pack (show e)}

 t1<- liftIO$ getCurrentTime

 context <- liftIO$ OSX.runActions OSX.currentApplication

 let actions = (vSettings&vConfig&vDesugar) context value
 liftIO$ OSX.runActionsWithDelay 5 actions
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called
 t2<- liftIO$ getCurrentTime

 let d1 = (1000 * (t1 `diffUTCTime` t0))
 let d2 = (1000 * (t2 `diffUTCTime` t1))

 liftIO$ do
  putStrLn""
  putStrLn$ "ACTIONS:"
  putStr  $ OSX.showActions actions
  putStrLn ""
  putStrLn$ "TIMES:"
  putStrLn$ show d1
  putStrLn$ show d2
  putStrLn ""
  putStrLn$ "CONTEXT:"
  print context
  putStrLn ""
  putStrLn$ "VALUE:"
  print value
  putStrLn ""
  putStrLn$ "WORDS:"
  T.putStrLn$ T.intercalate (T.pack " ") ws
  replicateM_ 3 (putStrLn"")

  -- performMinorGC
  performMajorGC

