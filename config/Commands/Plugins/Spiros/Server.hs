{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, ViewPatterns, OverloadedStrings                         #-}
{-| (very hacky for now) 

-}
module Commands.Plugins.Spiros.Server where 
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Root
import           Commands.Plugins.Spiros.Phrase.Types 
import           Commands.Plugins.Spiros.Shim (getShim)
import           Commands.Plugins.Spiros.Server.Types 
import           Commands.Plugins.Spiros.Server.Workflow 
import           Commands.Plugins.Spiros.Correct 

import           Commands.Parsers.Earley (EarleyParser(..), bestParse, eachParse) 
import qualified Commands.Backends.OSX         as OSX
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd) 
import           Commands.Servers.Servant

import           Control.Lens 
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant
import System.Clock 
import           Data.List.NonEmpty              (NonEmpty (..))
import Control.Monad.Free.Church (fromF) 

import Data.Char
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad

import System.IO
import System.Mem
import qualified Data.List as List 
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent
-- import Control.Exception 


-- ================================================================ --

spirosServer :: IO ()
spirosServer = do
 globals <- newVGlobals
 serveNatlink (spirosSettings globals rootsCommand)
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
 globals <- newVGlobals
 status <- (bool2exitcode . either2bool) <$> spirosSetup (spirosSettings globals rootsCommand) -- lazy 
 -- spirosInterpret spirosMagic rankRoots (spirosSettings globals rootsCommand) (RecognitionRequest ["test"]) -- TODO give name to"test" 
 exitWith status 


-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
spirosSettings
 :: VGlobals
 -> DNSEarleyCommand SpirosType
 -> VSettings OSX.CWorkflow SpirosType
spirosSettings spirosGlobals command = VSettings
 8888
 (spirosSetup )
 (spirosInterpret spirosMagic rankRoots)
 (spirosHypotheses ) 
 (spirosCorrection ) 
 (spirosUpdateConfig spirosDnsOptimizationSettings command)
 (Address localhost (Port 8889))       -- TODO Commands.Plugins.Spiros.Port 
 spirosGlobals

newVGlobals :: IO VGlobals  -- TODO rename Spiros to my 
newVGlobals = do
 vResponse <- atomically (newTVar emptyDNSResponse) 
 vMode <- atomically (newTVar RecognitionMode) 
 return VGlobals{..} 

spirosDnsOptimizationSettings :: DnsOptimizationSettings
spirosDnsOptimizationSettings = defaultDnsOptimizationSettings
 & dnsOptimizeInlineSmall .~ True
 -- & .~ 


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
spirosUpdateConfig
 :: DnsOptimizationSettings
 -- -> RULED (VSettings OSX.CWorkflow) r a
 -> DNSEarleyCommand a
 -> VConfig OSX.CWorkflow a
spirosUpdateConfig dnsSettings command = VConfig
 (unsafeDNSGrammar dnsSettings (command&_cRHS))
 (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
 (command&_cDesugar)
{-# NOINLINE spirosUpdateConfig #-}


spirosSetup
 :: VSettings OSX.CWorkflow a
 -> IO (Either VError ())
spirosSetup vSettings = do
 hSetBuffering stdout LineBuffering  -- a parser failure would exit in (EitherT IO), not printing the tokens or the error message

 let address = Address (Host "192.168.56.1") (Port (vSettings&vPort))

 let theShim = applyShim getShim address (vSettings&vConfig&vGrammar)

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

  Right (PythonFile shim) -> do

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

   -- because:
   -- 1. when pasting into an editor in virtual box, the clipboard contents are often trailed by Unicode garbage
   -- 2. which is why the shim ends in a comment 
   -- 3. but Unicode characters can have nonlocal effects on other characters, like on previous lines   
   OSX.runWorkflow$ OSX.setClipboard (T.unpack (T.filter isAscii shim))

   T.putStrLn$ displayAddress address
   putStrLn ""

   putStrLn "SHIM PARSING SUCCESS" -- TODO logging

   return$ Right()


{- | this handler:

* supports short-circuiting (in 'EitherT') on parser error, returning an HTTP error status.
* executes the compiled actions (in 'IO').

-}
spirosInterpret
 :: (Show a)
 => ServerMagic a
 -> Ranking a
 -> VSettings OSX.CWorkflow a
 -> RecognitionRequest 
 -> Response (DNSResponse)
spirosInterpret serverMagic theRanking vSettings = \(RecognitionRequest ws) -> do

 let printCurrentApplication = OSX.runWorkflow OSX.currentApplication >>= print  

 threadA <- liftIO$ forkIO$ forever (putStr "A: " >> printCurrentApplication)
 threadB <- liftIO$ forkIO$ forever (putStr "B: " >> printCurrentApplication)

 t0<- liftIO$ getTime theClock 

 !value <- case bestParse (vSettings&vConfig&vParser) ws of
  -- for thunk for accurate profiling 
  Right x -> return x
  Left e -> do
   liftIO$ do
    replicateM_ 3 (putStrLn"")
    putStrLn$ "ERROR:"
    print e
    putStrLn$ "WORDS:"
    putStrLn$ showWords ws
    hFlush stdout
   left$ err400{errBody = BSC.pack (show e)}

 t1<- liftIO$ getTime theClock 

 context <- liftIO$ OSX.runWorkflow OSX.currentApplication

 let hParse = either2maybe . (bestParse (vSettings&vConfig&vParser))
 let hDesugar = fromF . ((vSettings&vConfig&vDesugar) context)
 let theHandlers = CommandsHandlers{..}

 let theAmbiguousParser theWords = either (const (Nothing, [])) (\(x:|xs) -> (Just ((vSettings&vConfig&vParser&pBest) (x:|xs)), (x:xs))) (eachParse (vSettings&vConfig&vParser&pProd) theWords) -- TODO 

 let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
 let workflowIO = OSX.runWorkflowWithDelay 5 workflow

 liftIO$ (atomically$ getMode (vSettings&vGlobals)) >>= \case 
   RecognitionMode -> workflowIO 
   CorrectionMode  -> workflowIO 
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called

 -- magic actions, TODO replace with a free monad
 shouldPrint <- liftIO$ (atomically$ getMode (vSettings&vGlobals)) >>= \case 
  RecognitionMode -> serverMagic theHandlers theAmbiguousParser theRanking ws value
  CorrectionMode  -> return False 

 t2<- liftIO$ getTime theClock 

 let d1 = diffTimeSpecAsMilliseconds t1 t0 
 let d2 = diffTimeSpecAsMilliseconds t2 t1 
 let d3 = diffTimeSpecAsMilliseconds t2 t0

 when shouldPrint $ liftIO$ do  -- TODO don't print but still log? 
     printHeader 
     putStrLn$ "MODE:"
     print =<< do atomically$ getMode (vSettings&vGlobals)
     putStrLn ""
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
     putStrLn$ showWords ws

 liftIO$ performMajorGC                -- TODO other thread and delayed ?

 liftIO$ do                     -- TODO test 
  killThread threadA
  killThread threadB

 dnsRespond vSettings

 where 
 theClock = Realtime


spirosHypotheses
 :: VSettings OSX.CWorkflow a
 -> HypothesesRequest 
 -> Response DNSResponse
spirosHypotheses vSettings = \hypotheses -> do
 liftIO$ handleHypotheses (vSettings&vUIAddress) (vSettings&vGlobals) hypotheses
 dnsRespond vSettings


spirosCorrection
 :: VSettings OSX.CWorkflow a
 -> CorrectionRequest 
 -> Response DNSResponse 
spirosCorrection vSettings = \(CorrectionRequest correction) -> do
 liftIO$ handleCorrection' correction
 dnsRespond vSettings


-- | on 'Ambiguous', print all parse results.  
spirosMagic :: ServerMagic Roots 
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


handleHypotheses :: Address -> VGlobals -> HypothesesRequest -> IO ()
handleHypotheses _address globals hypotheses@(HypothesesRequest hs) = do 
 printHeader 
 printMessage $ hypothesesMessage 

 _ <- forkIO$ do                                -- TODO should be singleton. use some thread manager? 
     atomically$ setMode globals CorrectionMode  -- TODO  bracket
     OSX.runWorkflow$ reachCorrectionUi
     promptCorrection hypotheses >>= handleCorrection globals -- ignoring control C ? ask 
     OSX.runWorkflow$ unreachCorrectionUi
     atomically$ setMode globals RecognitionMode 

 return() 
 
 where 

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


handleCorrection :: VGlobals -> Dictation -> IO ()
handleCorrection globals theCorrection = do 
 let theResponse = (ForeignResultsObject 0 , (\(Dictation ws) -> fmap T.pack ws) theCorrection) -- TODO ForeignResultsObject 
 atomically$ writeCorrection globals (CorrectionResponse theResponse)
 printHeader 
 printMessage$ correctionMessage 

 where 

 correctionMessage =
  [ "CORRECTION:"
  ] <> [displayDictation theCorrection]


writeCorrection :: VGlobals -> CorrectionResponse -> STM () 
writeCorrection VGlobals{..} correction = do
 modifyTVar (vResponse) $ set (responseCorrection) (Just correction) 

setMode :: VGlobals -> VMode -> STM ()  
setMode VGlobals{..} mode = do 
 modifyTVar (vMode) $ set id mode 

getMode :: VGlobals -> STM VMode
getMode VGlobals{..} = readTVar vMode

dnsRespond :: VSettings m a -> Response DNSResponse
dnsRespond vSettings = do
 liftIO $ atomically $ do
   swapTVar (vSettings&vGlobals&vResponse) emptyDNSResponse

printHeader :: IO ()
printHeader = do 
 putStrLn"--------------------------------------------------------------------------------" 
 replicateM_ 3 (putStrLn"")

