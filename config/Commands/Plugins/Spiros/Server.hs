{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, ViewPatterns, OverloadedStrings                         #-}
{-| (very hacky for now) 

-}
module Commands.Plugins.Spiros.Server where 
import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Root
import           Commands.Plugins.Spiros.Shim (getShim)
import           Commands.Plugins.Spiros.Server.Workflow 

import qualified Commands.Backends.OSX         as OSX
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9
import           Commands.Servers.Servant

import           Control.Lens 
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8    as BSC
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Servant
import System.Clock 
import Data.List.NonEmpty (nonEmpty) 
import Control.Monad.Free.Church (fromF) 

import Data.Char
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import           Control.Monad.ST.Unsafe
import           System.IO.Unsafe
import System.IO
import System.Mem
import Control.Arrow ((&&&)) 
import qualified Data.List as List 
import Data.Monoid ((<>))
import System.Exit


type ServerMagic a = CommandsHandlers a OSX.Workflow_ -> AmbiguousParser a -> Ranking a -> [Text] -> a -> IO Bool -- TODO

type AmbiguousParser a = [Text] -> (Maybe a, [a])



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


spirosTest :: IO () 
spirosTest = do 
 status <- (bool2exitcode . either2bool) <$> spirosSetup (spirosSettings rootsCommand) -- lazy 
 exitWith status 


-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
spirosSettings
 :: RULED DNSEarleyCommand r SpirosType
 -> RULED (VSettings OSX.CWorkflow) r SpirosType
spirosSettings command = VSettings
 8888
 (spirosSetup )
 (spirosInterpret spirosMagic rankRoots)
 (spirosHypotheses ) 
 (spirosUpdateConfig spirosDnsOptimizationSettings command)


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
 -> RULED DNSEarleyCommand r a
 -> RULED (VConfig OSX.CWorkflow) r a
spirosUpdateConfig dnsSettings command = unsafePerformIO$ do
 vGrammar <- de'deriveGrammarObservedSharing dnsSettings (command&_cRHS)
 -- let eProd = runST$ de'deriveParserObservedSharing (command&_cRHS)
 eProd <- unsafeSTToIO$ de'deriveParserObservedSharing (command&_cRHS) --TODO runST, but input is not rank2
 let vParser = EarleyParser eProd (command&_cBest)
 let vDesugar = (command&_cDesugar)
 return VConfig{..}
{-# NOINLINE spirosUpdateConfig #-}


spirosSetup
 :: RULED (VSettings OSX.CWorkflow) r a
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
 -> (forall r. RULED (VSettings OSX.CWorkflow) r a)
 -> [Text]
 -> Response ()
spirosInterpret serverMagic theRanking vSettings = \ws -> do

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
    putStrLn$ showWords ws
    hFlush stdout
   left$ err400{errBody = BSC.pack (show e)}

 t1<- liftIO$ getTime theClock 

 context <- liftIO$ OSX.runWorkflow OSX.currentApplication

 let hParse = either2maybe . (e'ParseBest (vSettings&vConfig&vParser))
 let hDesugar = fromF . ((vSettings&vConfig&vDesugar) context)
 let theHandlers = CommandsHandlers{..}

 let theAmbiguousParser theWords = ((fmap (vSettings&vConfig&vParser&pBest) . nonEmpty) &&& id) (e'ParseList (vSettings&vConfig&vParser&pProd) theWords)

 let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
 let workflowIO = OSX.runWorkflowWithDelay 5 workflow
 liftIO$ workflowIO 
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called

 -- magic actions, TODO replace with a free monad
 shouldExecute <- liftIO$ serverMagic theHandlers theAmbiguousParser theRanking ws value

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
  putStrLn$ showWords ws

  -- performMinorGC
  performMajorGC

 where 
 theClock = Realtime


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


spirosHypotheses
 :: (forall r. RULED (VSettings OSX.CWorkflow) r a)
 -> [Hypothesis]
 -> Response ()
spirosHypotheses _settings hypotheses = do
 liftIO$ handleHypotheses hypotheses


type CommandsRequest = [Text]

data CommandsResponse a b = CommandsResponse 
 { rRaw       :: [Text]
 , rParsed    :: Maybe a 
 , rDesugared :: Maybe b 
 }

data CommandsHandlers a b = CommandsHandlers
 { hParse   :: [Text] -> Maybe a 
 , hDesugar :: a -> b 
 }

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


leftAppendLineNumbers :: Text -> (Int,Int,Text) 
leftAppendLineNumbers code = (marginWidth, countWidth, (T.unlines . imap go) allLines)
 where 
 go ((+1) -> lineNumber) oneLine = getLeftMargin lineNumber <> oneLine 
 marginWidth = (fromInteger . toInteger . T.length) (getLeftMargin (0::Integer))  -- assumes the length is constant 
 getLeftMargin lineNumber = "[" <> T.pack (padNumber countWidth lineNumber) <> "]"
 countWidth = length (show lineCount)
 lineCount = length allLines 
 allLines = T.lines code


handleHypotheses :: [Hypothesis] -> IO ()
handleHypotheses hypotheses = do 
 putStrLn$ (List.intercalate "\n") message

 where 
 message =
  [ "" 
  , "" 
  , "HYPOTHESES:"
  ] <> (imap showHypothesis hypotheses) 

 showHypothesis ((+1) -> index_) hypothesis = 
  show index_ <> ". " <> T.unpack (T.intercalate " " hypothesis) 


