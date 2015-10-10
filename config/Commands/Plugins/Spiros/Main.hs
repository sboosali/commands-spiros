{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, ViewPatterns                        #-}
module Commands.Plugins.Spiros.Main where 
import           Commands.Plugins.Spiros.Etc
import           Commands.Plugins.Spiros.Root
import           Commands.Plugins.Spiros.Shim (getShim)

import qualified Commands.Backends.OSX         as OSX
import           Commands.Etc
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant

import           Control.Lens                  hiding (from, ( # ))
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant
import System.Clock 
import Data.List.NonEmpty (nonEmpty) 

import Data.Char
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe
import System.IO
import System.Mem
import Control.Arrow ((&&&)) 


type SpirosType = Roots

type ServerMagic a = (([Text] -> (Maybe a, [a])) -> [Text] -> a -> IO (Bool)) -- TODO


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

-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
spirosSettings :: RULED DNSEarleyCommand r SpirosType -> RULED VSettings r SpirosType
spirosSettings command = VSettings 8888 spirosSetup (spirosInterpret spirosMagic) (spirosUpdateConfig command)

-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
-- spirosSettings command = VSettings 8888 spirosSetup (spirosInterpret (\_ _ _ -> return())) (spirosUpdateConfig command)

-- spirosSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- spirosSettings plugin = (defSettings runWorkflow spirosUpdateConfig plugin)
--   { vSetup = spirosSetup
--   }

-- spirosSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- spirosSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runWorkflow spirosUpdateConfig plugin)
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

 let theShim = applyShim getShim address (settings&vConfig&vGrammar)

 case theShim of 
  Left (PythonSyntaxError e s) -> do
   putStrLn ""
   T.putStrLn s
   putStrLn ""
   print e
   putStrLn ""
   return$ Left(VError "")

  Right (PythonFile shim) -> do
   putStrLn "" -- TODO logging

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

   OSX.runWorkflow$ OSX.setClipboard (T.unpack (T.filter isAscii shim))

   T.putStrLn$ displayAddress address
   putStrLn ""

   return$ Right()

{- | this handler:

* supports short-circuiting (in 'EitherT') on parser error, returning an HTTP error status.
* executes the compiled actions (in 'IO').

-}
spirosInterpret :: (Show a) => ServerMagic a -> (forall r. RULED VSettings r a) -> [Text] -> Response ()
spirosInterpret serverMagic vSettings = \ws -> do

 t0<- liftIO$ getTime theClock 

 !value <- case e'ParseBest (vSettings&vConfig&vParser) ws of
  -- for thunk for accurate profiling 
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

 t1<- liftIO$ getTime theClock 

 context <- liftIO$ OSX.runWorkflow OSX.currentApplication

 let workflow = (vSettings&vConfig&vDesugar) context value  -- return() 
 let workflowIO = OSX.runWorkflowWithDelay 5 workflow
 liftIO$ workflowIO 
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called

 -- magic actions, TODO replace with a free monad
 let parser_ ws_ = ((fmap (vSettings&vConfig&vParser&pBest) . nonEmpty) &&& id) (e'ParseList (vSettings&vConfig&vParser&pProd) ws_)
 shouldExecute <- liftIO$ serverMagic parser_ ws value 

 t2<- liftIO$ getTime theClock 

 let d1 = diffTimeSpecAsMilliseconds t1 t0 
 let d2 = diffTimeSpecAsMilliseconds t2 t1 
 let d3 = diffTimeSpecAsMilliseconds t2 t0

 when shouldExecute $ liftIO$ do
  replicateM_ 3 (putStrLn"")
  putStrLn$ "WORKFLOW:"
  putStr  $ OSX.showWorkflow workflow
  putStrLn ""
  putStrLn$ "TIMES:"
  putStrLn$ show d1 ++ "ms"
  putStrLn$ show d2 ++ "ms"
  putStrLn$ show d3 ++ "ms"
  putStrLn ""
  putStrLn$ "CONTEXT:"
  print context
  putStrLn ""
  putStrLn$ "VALUE:"
  print value
  putStrLn ""
  putStrLn$ "WORDS:"
  T.putStrLn$ T.intercalate (T.pack " ") ws

  -- performMinorGC
  performMajorGC

 where 
 theClock = Realtime


-- | on 'Ambiguous', print all parse results.  
spirosMagic :: ServerMagic Roots 
spirosMagic parse_ ws = \case 

  Ambiguous _r -> case ws of
   ((T.unpack -> "explicate"):ws_) -> liftIO$ do -- TODO grammatical symbol is hardcoded 
    let (value,values) = parse_ ws_
    replicateM_ 3 (putStrLn"")

    putStrLn$ "LENGTH:" 
    print   $ length values 
    putStrLn$ "" 

    putStrLn$ "WORDS:"
    T.putStrLn$ T.intercalate (T.pack " ") ws
    putStrLn$ "" 

    putStrLn$ "BEST:"
    print   $ value 
    putStrLn$ "" 

    putStrLn$ "VALUES:"
    itraverse_ printValue values 

    return False 

   _ -> return True 

  _ -> return True 

 where 
 printValue ((+1) -> index_) value = do 
  putStrLn ""
  print $ show index_ ++ "." 
  print value

