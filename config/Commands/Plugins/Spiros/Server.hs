{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, ViewPatterns, OverloadedStrings, TupleSections                          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-| (very hacky for now) 

-}
module Commands.Plugins.Spiros.Server where 
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Server.Settings 
import           Commands.Plugins.Spiros.Types 
import           Commands.Plugins.Spiros.Root
import           Commands.Plugins.Spiros.Phrase.Types 
import           Commands.Plugins.Spiros.Shim (cleanShim, getShim)
import           Commands.Plugins.Spiros.Windows 
import           Commands.Plugins.Spiros.Server.Types 
import           Commands.Plugins.Spiros.Server.Workflow 
import           Commands.Plugins.Spiros.Server.QQ
import           Commands.Plugins.Spiros.Correct 

import           Commands.Parsers.Earley (EarleyParser(..), bestParse, eachParse) 
import qualified Commands.Backends.OSX         as OSX
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd) 
import           Commands.Servers.Servant

import           Control.Lens 
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import System.Clock 
import           Data.List.NonEmpty              (NonEmpty (..))
import Control.Monad.Free.Church (fromF) 
import Control.DeepSeq(force) 

-- import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import System.IO
import System.Mem
import qualified Data.List as List 
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent
import           Control.Monad.Reader 
import           Control.Monad.Except 
import           Control.Exception(bracket_) 


-- ================================================================ --

spirosServer :: IO ()
spirosServer = do
 (settings, globals, _reference) <- newSpirosSettings rootsCommand

 _theContextThread <- forkContextWorker globals  -- TODO manage threads 
 _theModeThread <- forkModeWorker globals        -- TODO pipes-mvc? 

 natlinkIO settings 

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

spirosTest :: IO () 
spirosTest = do 
 (settings, globals, _reference) <- newSpirosSettings rootsCommand

 _theContextThread <- forkContextWorker globals  -- TODO manage threads 

 theEnvironment <- getVEnvironment settings   
 setupStatus     <- either2bool <$> spirosSetup theEnvironment 

 -- TODO printHeader 
 -- let _runInterpret = runEitherT (spirosInterpret spirosMagic rankRoots (newSpirosSettings  globals rootsCommand) (RecognitionRequest ["test"]))   -- NOTE it's a NULLOP.  TODO give name to"test" testInterpret 
 -- let _handleInterpret = \case; Left e -> (do print e >> return (Left e)); Right x -> (do print x >> return (Right x)) 
 -- interpretStatus <- either2bool <$> (_runInterpret >>= _handleInterpret)
 let interpretStatus = True 

 let theStatus = (setupStatus && interpretStatus) 
 printHeader 
 if theStatus then putStrLn "spirosTest: Success" else putStrLn "spirosTest: FAILURE" 
 exitWith (bool2exitcode theStatus) 


-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
newSpirosSettings 
 :: SpirosCommand 
 -> IO (SpirosSettings, SpirosGlobals, TVar SpirosPlugin)   
newSpirosSettings spirosCommand = do 

 spirosGlobals <- newVGlobals GlobalContext 
 spirosPlugin <- newVPlugin $ spirosUpdate spirosDnsOptimizationSettings spirosCommand

 return$ (makeSpirosSettings spirosGlobals spirosPlugin, spirosGlobals, spirosPlugin)  


makeSpirosSettings spirosGlobals spirosPluginReference  = VSettings
 (spirosSetup )
 (VConfig spirosSettings_ spirosBackend spirosGlobals)
 spirosPluginReference

 (spirosInterpret spirosMagic rankRoots)
 (spirosHypotheses ) 
 (spirosCorrection ) 
 (spirosReload  ) 
 (spirosContext  ) 



newVGlobals :: c -> IO (VGlobals c)
newVGlobals c = atomically$ do
 vResponse <- newTVar emptyDNSResponse 
 vMode <- newTVar NormalMode
 vContext <- newTVar c 
 return VGlobals{..} 


newVPlugin :: VPlugin m c v -> IO (TVar (VPlugin m c v))
newVPlugin x = atomically$ newTVar x


spirosBackend = VBackend{..} 
 where
 vExecute = fromF >>> OSX.runWorkflowWithDelay 5 


-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
-- spirosSettings command = VSettings 8888 spirosSetup (spirosInterpret (\_ _ _ -> return())) (spirosUpdate command)

-- spirosSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- spirosSettings plugin = (defSettings runWorkflow spirosUpdate plugin)
--   { vSetup = spirosSetup
--   }

-- spirosSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- spirosSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runWorkflow spirosUpdate plugin)
--  return$ settings
--   { vSetup = setupCopyGrammar :: (VSettings (E.Rule r Root) Root -> IO (Either VError ()))
--   }


-- spirosUpdate :: VPlugin (E.Rule r Root) Root -> IO (VConfig (E.Rule r Root) Root)
spirosUpdate
 :: DnsOptimizationSettings
 -- -> RULED (SpirosSettings) r a
 -> SpirosCommand
 -> SpirosPlugin 
spirosUpdate dnsSettings command = VPlugin
  (unsafeDNSGrammar dnsSettings (command&_cRHS))
  (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
  (command&_cDesugar)
{-# NOINLINE spirosUpdate #-}


spirosSetup
 :: SpirosEnvironment 
 -> IO (Either VError ())
-- spirosSetup VEnvironment{ePlugin{..},eConfig{..}} = do
spirosSetup environment = do 
 let theConfig = environment& eConfig&vSettings_&vNatLinkSettings
 let theGrammar = (environment &ePlugin&vGrammar)
 let address = theConfig &nlAddress -- TODO theAddress 

 do   
   putStrLn ""
   T.putStrLn$ displayAddress address
   T.putStrLn$ curl_ExampleRequest address 

 do
   putStrLn ""
   T.putStrLn$ getBatchScript myBatchScriptR
   setClipboardIO$ T.unpack (getBatchScript myBatchScriptR)
   putStrLn ""
   putStrLn$ getBatchScriptPath myBatchScriptR

 let theShim = fmap (over _PythonFile cleanShim) $ applyShim getShim theConfig theGrammar  -- TODO is this the right place? 

 case theShim of 

  Left (PythonSyntaxError e s) -> do
   let (errorRow, errorColumn) = getPythonErrorSpan e
   let (marginWidth, countWidth, code) = leftAppendLineNumbers s

   putStrLn ""
   print e
   putStrLn ""
   T.putStrLn$ code 
   putStrLn ""
   OSX.runWorkflow $ findErrorBySearch countWidth marginWidth errorRow errorColumn 
   putStrLn "SHIM PARSING FAILURE" -- TODO logging
   return$ Left(VError "")

  Right myShim -> do

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

   -- because:
   -- 1. when pasting into an editor in virtual box, the clipboard contents are often trailed by Unicode garbage
   -- 2. which is why the shim ends in a comment 
   -- 3. but Unicode characters can have nonlocal effects on other characters, like on previous lines   
   copyShim myShim
   writeShim myBatchScriptR myShim -- TODO its own function 

   putStrLn "SHIM PARSING SUCCESS" -- TODO logging

   return$ Right()


{- | this handler:

* supports short-circuiting (in 'EitherT') on parser error, returning an HTTP error status.
* executes the compiled actions (in 'IO').

-}
-- spirosInterpret
--  :: (NFData a, Show a)
--  => ServerMagic a
--  -> Ranking a
--  -> RecognitionRequest 
--  -> SpirosResponse 
spirosInterpret serverMagic theRanking = \(RecognitionRequest ws) -> do

 VEnvironment{..} <- ask 

 t0<- getTime_ 

 !(force -> value) <- case bestParse (ePlugin&vParser) ws of
  -- {force} turns WHNF (the bang pattern) into NF
  Right x -> return x 
  Left e -> do
   liftIO$ do
    replicateM_ 3 (putStrLn"")
    putStrLn$ "ERROR:"
    print e
    putStrLn$ "WORDS:"
    putStrLn$ showWords ws
    hFlush stdout
   throwError$ VError (show e) 

 context <- liftIO$ readSpirosContext <$> OSX.runWorkflow OSX.currentApplication

 let hParse = either2maybe . (bestParse (ePlugin&vParser))
 let hDesugar = fromF . ((ePlugin&vDesugar) context)
 let theHandlers = CommandsHandlers{..}

 let theAmbiguousParser = makeAmbiguousParser (ePlugin&vParser)

 let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
 let workflowIO = OSX.runWorkflowWithDelay 5 workflow

 t2<- getTime_

 liftIO$ (atomically$ getMode (eConfig&vGlobals)) >>= \case 
   NormalMode -> workflowIO 
   CorrectingMode -> workflowIO 
   m -> print m 
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called

 -- magic actions, TODO replace with a free monad
 shouldPrint <- liftIO$ (atomically$ getMode (eConfig&vGlobals)) >>= \case 
   NormalMode -> serverMagic theHandlers theAmbiguousParser theRanking ws value
   CorrectingMode  -> return False 
   _ -> return True 

 let d3 = diffTimeSpecAsMilliseconds t2 t0

 when shouldPrint $ liftIO$ do  -- TODO don't print but still log? 
     printHeader 
     putStrLn$ "RESPONSE:" 
     print =<< do atomically$ readTVar (eConfig&vGlobals&vResponse) -- TODO not exactly the same as dnsRespond. should be constant throughout request?  
     putStrLn ""
     putStrLn$ "MODE:"
     print =<< do atomically$ getMode (eConfig&vGlobals)
     putStrLn ""
     putStrLn$ "WORKFLOW:"
     putStr  $ OSX.showWorkflow workflow
     putStrLn ""
     putStrLn$ "TIMES:"
     putStrLn$ show d3 ++ "ms"
     putStrLn ""
     putStrLn$ "CONTEXT:"
     print =<< do atomically$ getContext (eConfig&vGlobals)
     putStrLn ""
     putStrLn$ "APPLICATION:"
     print context
     putStrLn ""
     putStrLn$ "VALUE:"
     print value
     putStrLn ""
     putStrLn$ "WORDS:"
     putStrLn$ showWords ws

 liftIO$ performMajorGC                -- TODO other thread and delayed ?

 dnsRespond 

 where 
 getTime_ = liftIO$ getTime Realtime


-- spirosHypotheses
--  :: VSettings_
--  -> SpirosGlobals 
--  -> HypothesesRequest 
--  -> SpirosResponse
-- spirosHypotheses VSettings_{..} vGlobals = \hypotheses -> do
 -- liftIO$ handleHypotheses (vUIAddress) vGlobals hypotheses
spirosHypotheses :: SpirosHandler HypothesesRequest
spirosHypotheses = \hypotheses -> do
 globals <- asks (eConfig>>>vGlobals) 
 liftIO$ handleHypotheses globals hypotheses
 dnsRespond


-- spirosCorrection
--  :: CorrectionRequest 
--  -> SpirosResponse 
spirosCorrection = \(CorrectionRequest correction) -> do
 liftIO$ handleCorrection' correction
 dnsRespond 


-- spirosReload
--  :: ReloadRequest 
--  -> SpirosResponse 
spirosReload = \() -> do
 liftIO$ handleReload
 dnsRespond 


-- spirosContext
--  :: ContextRequest 
--  -> SpirosResponse
spirosContext = \() -> do
 liftIO$ handleContext
 dnsRespond 


-- | on 'Ambiguous', print all parse results.  
spirosMagic :: ServerMagic SpirosType 
spirosMagic theHandlers theAmbiguousParser theRanking theWords = \case 

  Frozen (List.nub -> List.sort -> stages) _r -> do
   replicateM_ 2 (putStrLn"")

   case theWords of
    ((T.unpack -> RootsFrozenPrefix):ws) -> do -- TODO grammatical symbol is hardcoded 
        let theResponse = handleRequest theHandlers ws 
        traverse_ (handleStage theResponse) stages 
        return False 

    _ -> return True 

  Ambiguous _ -> case theWords of
   ((T.unpack -> RootsAmbiguousPrefix):ws) -> do -- TODO grammatical symbol is hardcoded 
    liftIO$ handleParses theAmbiguousParser theRanking ws 
    return False 
   _ -> return True 

  _ -> return True 


{-| the fields in the output are constructed lazily, with intermediary computations shared. 

-}
handleRequest :: CommandsHandlers a b -> CommandsRequest -> CommandsResponse a b  
handleRequest CommandsHandlers{..} ws = CommandsResponse{..} 
 where
 rRaw       = ws
 rParsed    = hParse rRaw
 rDesugared = hDesugar <$> rParsed


printStage :: (Show a) => CommandsResponse a OSX.Workflow_ -> Stage -> IO() 
printStage CommandsResponse{..} = \case 
 RawStage   -> do 
  putStrLn ""
  putStrLn "WORDS:"
  putStrLn$ showWords rRaw
 ParseStage -> do 
  putStrLn ""
  putStrLn "VALUE:"
  traverse_ print rParsed
 RunStage   -> do 
  putStrLn ""
  putStrLn "WORKFLOW:" 
  traverse_ printWorkflow rDesugared


handleStage :: (Show a) => CommandsResponse a OSX.Workflow_ -> Stage -> IO ()  
handleStage CommandsResponse{..}= \case 

         RawStage   -> do 
             putStrLn ""
             putStrLn "WORDS:"
             printAndPaste (showWords rRaw)

         ParseStage -> do 
             putStrLn ""
             putStrLn "VALUE:"
             traverse_ (printAndPaste . show) rParsed

         RunStage   -> do 
             putStrLn ""
             putStrLn "WORKFLOW:" 
             traverse_ (printAndPaste . OSX.showWorkflow) rDesugared 

handleParses
 :: (Show a)
 => AmbiguousParser a -> Ranking a -> [Text] -> IO ()
handleParses theParser theRanking ws = do
 let (value,values) = theParser ws
 let message = [ ""
               , "" 
               , "" 

               , "LENGTH:" 
               , show (length values) 
               , "" 

               , "WORDS:"
               , showWords ws 
               , "" 

               , "BEST:"
               , show value 
               , "" 

               , "VALUES:"
               ] <> concat (imap showValue values)

 printAndPaste (List.intercalate "\n" message)

 where 
 showValue ((+1) -> index_) value =
  [ ""
  , (show index_ <> ".")
  , ("(" <> show (theRanking value) <> ")")
  , show value
  ]


-- handleHypotheses :: Address -> VGlobals c -> HypothesesRequest -> IO ()
-- handleHypotheses _address globals hypotheses@(HypothesesRequest hs) = do 
handleHypotheses globals hypotheses@(HypothesesRequest hs) = do 

 printHeader 
 printMessage $ hypothesesMessage 

 _ <- forkIO$ do                                -- TODO should be singleton. use some thread manager? 
     bracket_ openCorrection closeCorrection useCorrection 

 return() 
 
 where 

 openCorrection = do 
  atomically$ setMode globals CorrectingMode
  OSX.runWorkflow$ reachCorrectionUi

 closeCorrection = do 
  atomically$ setMode globals NormalMode
  OSX.runWorkflow$ unreachCorrectionUi

 useCorrection = do 
  promptCorrection hypotheses >>= handleCorrection globals -- ignoring control C ? ask 

 hypothesesMessage =
  [ "HYPOTHESES:"
  , "" 
  ] <> showHypotheses hs 

 showHypotheses = \case
  [] -> [] 
  (theRecognition:theHypotheses) -> concat
   [ fmap (" " <>) (imap showHypothesis theHypotheses) 
   , ["(" <> showHypothesis (-1::Int) theRecognition <> ")"] 
   ]

 showHypothesis ((+1) -> index_) hypothesis = 
  show index_ <> ". " <> T.unpack (T.intercalate " " hypothesis) 


handleCorrection' :: [Text] -> IO ()
handleCorrection' _correction = do    -- TODO 
 return()                    


handleCorrection :: VGlobals c -> Dictation -> IO ()
handleCorrection globals theCorrection = do 
 let theResponse = (ForeignResultsObject 0 , (\(Dictation ws) -> fmap T.pack ws) theCorrection) -- TODO ForeignResultsObject 
 atomically$ writeCorrection globals (CorrectionResponse theResponse)
 printHeader 
 printMessage$ correctionMessage 

 where 

 correctionMessage =
  [ "CORRECTION:"
  ] <> [displayDictation theCorrection]


{-| 
an invasive but simple way to signal to the user that the client has been reloaded. 
assumes that the "/context" endpoint is only called on module reloading. 

-}
handleReload :: IO ()
handleReload = do    -- TODO 
 OSX.runWorkflow$ reachLoggingUi
 printHeader 
 putStrLn "RELOADED" 
 return()                    


{-| 
-}
handleContext :: IO ()
handleContext = do    -- TODO 
 return()                    


writeCorrection :: VGlobals c -> CorrectionResponse -> STM () 
writeCorrection VGlobals{..} correction = do
 modifyTVar (vResponse) $ set (responseCorrection) (Just correction) 

writeContext :: (Show c) => VGlobals c -> STM ()           -- TODO 
writeContext globals@VGlobals{..} = do
 newContext <- show <$> getContext globals 
 modifyTVar vResponse $ set (responseContext) (Just newContext) 

writeMode :: VGlobals c -> STM ()           -- TODO 
writeMode globals@VGlobals{..} = do
 newMode <- getMode globals 
 modifyTVar vResponse $ set (responseVMode) (Just newMode) 

setMode :: VGlobals c -> VMode -> STM () 
setMode VGlobals{..} mode = do 
 modifyTVar (vMode) $ set id mode 

getMode :: VGlobals c -> STM VMode
getMode VGlobals{..} = readTVar vMode

setContext :: VGlobals c -> c -> STM ()  
setContext VGlobals{..} c = do 
 modifyTVar (vContext) $ set id c 

getContext :: VGlobals c -> STM c 
getContext VGlobals{..} = readTVar vContext 

dnsRespond :: SpirosResponse
dnsRespond = do 
 globals <- asks (eConfig>>>vGlobals) 
 liftIO $ atomically $ do
   swapTVar (globals&vResponse) emptyDNSResponse

printHeader :: IO ()
printHeader = do 
 putStrLn"--------------------------------------------------------------------------------" 
 replicateM_ 3 (putStrLn"")



-- ================================================================ --

type Worker = (Int, IO())          -- TODO 

forkWorker :: Worker -> IO ThreadId 
forkWorker = forkIO . forever . runWorker

runWorker :: Worker -> IO () 
runWorker (_delay, _action) = _action >> threadDelay _delay 


forkContextWorker :: SpirosGlobals -> IO ThreadId 
forkContextWorker globals = forkWorker (loadContextWorker globals) 
 
loadContextWorker :: SpirosGlobals -> Worker 
loadContextWorker globals = (milliseconds 10, loadContext globals) 

loadContext :: SpirosGlobals -> IO ()
loadContext globals = do 
 theApplication <- OSX.runWorkflow OSX.currentApplication 
 let theContext = readSpirosContext theApplication
 atomically$ setContext globals theContext 
 atomically$ writeContext globals -- hacky 


forkModeWorker :: SpirosGlobals -> IO ThreadId 
forkModeWorker globals = forkWorker (loadModeWorker globals) 

loadModeWorker :: SpirosGlobals -> Worker 
loadModeWorker = (milliseconds 10,) . loadMode

loadMode :: SpirosGlobals -> IO ()
loadMode globals = do 
 atomically$ writeMode globals -- hacky 


-- ================================================================ --

makeAmbiguousParser :: (forall s r. EarleyParser s r e t a) -> [t] -> (Maybe a, [a])
makeAmbiguousParser p theWords = either (const (Nothing, [])) (\(x:|xs) -> (Just ((p&pBest) (x:|xs)), (x:xs))) (eachParse (p&pProd) theWords) -- TODO 

-- spirosInterpreter :: (forall s r. EarleyParser s r e t a) -> [t] -> Either e a 
-- spirosInterpreter d p ws = do 
--  !(force -> value) <- bestParse p ws 
--   -- {force} turns WHNF (from the bang pattern) into NF

--  context <- liftIO$ readSpirosContext <$> OSX.runWorkflow OSX.currentApplication

--  let hParse = either2maybe . (bestParse p)
--  let hDesugar = fromF . (d context)
--  let theHandlers = CommandsHandlers{..}

--  let theAmbiguousParser = makeAmbiguousParser p

--  let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
--  let workflowIO = OSX.runWorkflowWithDelay 5 workflow

--  return workflowIO 
