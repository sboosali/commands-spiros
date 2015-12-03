{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances, ViewPatterns, OverloadedStrings, ScopedTypeVariables, ConstraintKinds, NamedFieldPuns, NoMonomorphismRestriction   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures -fno-warn-orphans #-}
module Commands.Plugins.Spiros.Extra
 ( module Commands.Plugins.Spiros.Extra
 , module Commands.Plugins.Spiros.Extra.Types 
 , module Commands.Extra
 , module Data.Semigroup
 ) where

import Commands.Plugins.Spiros.Types
import Commands.Plugins.Spiros.Extra.Types 

import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX
import           Commands.Extra -- for reexport 

import qualified System.FilePath.Posix as FilePath
import System.Clock (TimeSpec,timeSpecAsNanoSecs,diffTimeSpec) 
import Language.Python.Common.Token
import Language.Python.Common.SrcLocation
import Language.Python.Common.ParseError
import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 
import           Control.Lens(imap)  
import Data.Semigroup ((<>))

import Data.Foldable
import qualified Data.List as List
import System.Exit(ExitCode(..)) 
import           GHC.Exts                        (IsString)
import Text.Printf (printf) 
import System.IO
import System.Process 


nothing :: (Monad m) => m ()
nothing = return ()

whenJust :: (Monad m) => Maybe a -> (m () -> m ())
whenJust condition_ action_ = ifJust condition_ action_ nothing 

ifJust :: (Monad m) => Maybe b -> m a -> m a -> m a
ifJust condition_ actionTrue_ actionFalse_ = maybe (actionFalse_) (const actionTrue_) condition_


-- ================================================================ --
-- debugging 

-- import           Commands.Frontends.Dragon13
-- import           Commands.Plugins.Example.Spacing

-- import           Control.Lens hiding (from, ( # ))
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import           Data.List.NonEmpty (NonEmpty (..))
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as T

-- import           Control.Concurrent.Async
-- import           System.Timeout (timeout)

-- -- it seems to be synchronous, even with threaded I guess?
-- attemptAsynchronously :: Int -> IO () -> IO ()
-- attemptAsynchronously seconds action = do
--  (timeout (seconds * round (1e6::Double)) action) `withAsync` (waitCatch >=> \case
--    Left error     -> print error
--    Right Nothing  -> putStrLn "..."
--    Right (Just _) -> return ()
--   )

-- attempt = attemptAsynchronously 1

-- -- pseudo HTML ordered list
-- ol xs = ifor_ xs $ \i x -> do
--  putStrLn ""
--  putStrLn $ fold [show i, ". ", x]

-- attemptParse :: (Show a) => (forall  z. DNSEarleyRHS z a) -> String -> IO ()
-- attemptParse rule s = do
--  putStrLn ""
--  attempt $ parseThrow rule ((T.words . T.pack) s) >>= \case
--   x :| _ -> print x

-- attemptSerialize rhs = attemptAsynchronously 3 $ do
--  serialized <- formatRHS rhs
--  either print printSerializedGrammar serialized

-- printSerializedGrammar SerializedGrammar{..} = do
--  replicateM_ 3 $ putStrLn ""
--  T.putStrLn $ displayDoc serializedRules
--  putStrLn ""
--  T.putStrLn $ displayDoc serializedLists

-- main = do
--  putStrLn ""
--  let rootG = root
--  attemptSerialize rootG
--  attemptMunge "par round grave camel lit with async break break action"




-- ================================================================ --

{- | returns the digits that make up a number in some base, most-significant digit first. 

>>> toDigits 10 9
[9]

>>> toDigits 10 1234
[1,2,3,4]

assumes nonnegative input, but still is total. 

prop> \(Positive k) n -> ('fromDigits' k . toDigits k) n == n

-}
toDigits :: (Integral a) => a -> a -> [a]
toDigits base = reverse . List.unfoldr go
 where
 go n = if n < 1 then Nothing else Just ((n `mod` base), (n `div` base))

{- | returns the digits that make up a number in some base, most-significant digit first. 

>>> fromDigits 10 [1,2,3,4]
1234

>>> fromDigits 10 [9]
9

>>> fromDigits 10 []
0

-}
fromDigits :: forall a. (Integral a) => a -> [a] -> a
fromDigits base = foldr (+) 0 . zipWith (\i n -> (base^i) * n) [(0::a)..] . reverse

type Number = Int

-- average xs = realToFrac (sum xs) / genericLength xs
safeAverage :: Foldable t => t Int -> Int
safeAverage (toList -> []) = 0
safeAverage xs = sum xs `div` length xs

-- slot :: MonadWorkflow m => String -> m () 
slot s = do
 delay 10
 sendText s
 press "<ret>" 

isDefaultBrowser :: MonadWorkflow m => m (Maybe String)
isDefaultBrowser = currentApplication >>= \case
 x@"Google Chrome" -> return$ Just x
 _                 -> return$ Nothing

type Desugaring a = a -> CWorkflow_

type Ranking a = a -> Int

-- lawless, domain-specific, 'Ord'-like typeclass.
-- used by @data Apply@, permitting "ranking a function" by ranking its arguments (before application). 
-- can be derived, avoiding boilerplate. 
class Rankable a where          -- TODO a ranking that's relative, not absolute.
 rank :: Ranking a
 rank _ = defaultRank

defaultRank :: Int
defaultRank = 100

highRank :: Int
highRank = 1000

defaultRankMultiplier :: Int
defaultRankMultiplier = 1000

rankAtLeast :: Rankable a => Int -> a -> Int
rankAtLeast i a = min i (rank a) 

instance Rankable Int where rank = const defaultRank
instance Rankable Ordinal where rank = const defaultRank 
instance (Rankable a) => Rankable (Maybe a) where rank = rankMaybe
instance (Rankable a, Rankable b) => Rankable (Either a b) where rank = rankLeftBiased

-- instance Rankable (Either Shell Phrase) where rank = rankLeftBiased
-- instance Rankable (Either Ordinal Phrase) where rank = rankLeftBiased

rankMaybe :: (Rankable a) => Ranking (Maybe a)
rankMaybe = maybe defaultRank rank

rankLeftBiased :: (Rankable a, Rankable b) => Ranking (Either a b)
rankLeftBiased = either ((*defaultRankMultiplier) . rank) rank -- watch out, the multiplied rank pollutes any parents above it 
-- rankLeftBiased = either rank ((`div` defaultRankMultiplier) . rank)

isBrowser x = if FilePath.takeBaseName x `elem` ["Firefox", "Chrome"]
 then Just x
 else Nothing

defaultDelay = 100 :: Int

chromeDelay = 250 :: Int                 -- milliseconds
 -- in chrome, keypresses are lost when the delay isnt long enough 

browserDelay = chromeDelay

-- runRepeat :: (MonadWorkflow m) => Int -> Number -> (m () -> m ())
runRepeat delay_ times_
 = traverse_ id
 . List.intersperse (delay delay_)
 . replicate times_
 --TODO action grouping: insert nullop between each, for logging

-- used when sendText is too slow/laggy
-- insertByClipboard :: String -> AMonadAction_
-- insertByClipboard :: MonadWorkflow m => String -> m () 
insertByClipboard s = do
 setClipboard s
 presspaste

-- presspaste :: MonadWorkflow m => m () 
presspaste = press "M-v"

-- runs the action, then restores the previous clipboard contents. dictation still pollutes clipboard history, but the most recent "manual" clipboard contents should be preserved.  
-- benign race condition, as no lock is kept on the system clipboard
restoringClipboard :: Workflow a -> Workflow a
restoringClipboard m = do
 contents <- getClipboard
 x <- m
 delay 100 -- otherwise, e.g. the old clipboard contents are reset before the temporary clipboard contents are paste TODO call Haskell from Objective-C on callback? 
 setClipboard contents
 return x

diffTimeSpecAsMilliseconds :: TimeSpec -> TimeSpec -> Integer 
diffTimeSpecAsMilliseconds x y = (timeSpecAsNanoSecs (diffTimeSpec x y)) `div` (1000*1000)

either2maybe :: Either e a -> Maybe a 
either2maybe = either (const Nothing) Just 

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True) 

bool2exitcode :: Bool -> ExitCode 
bool2exitcode False = ExitFailure 1
bool2exitcode True  = ExitSuccess 

(.!!)   :: (t1 -> t2) -> (a -> b ->           t1) -> (a -> b ->           t2)
(.!!!)  :: (t1 -> t2) -> (a -> b -> c ->      t1) -> (a -> b -> c ->      t2)
(.!!!!) :: (t1 -> t2) -> (a -> b -> c -> d -> t1) -> (a -> b -> c -> d -> t2)

(.!!)   f g = \a b     -> f (g a b)
(.!!!)  f g = \a b c   -> f (g a b c)
(.!!!!) f g = \a b c d -> f (g a b c d)

{-| a type that supports string interpolation.

i.e. supports string literals (via 'IsString') and can be appended (via 'Monoid'). 

uses @ConstraintKinds@. 
  
-}
type CanInterpolate t = (IsString t, Monoid t)

padNumber :: Integral a => Int -> a -> String 
padNumber padding n = printf ("%0." ++ show padding ++ "d") (toInteger n) 

leftAppendLineNumbers :: Text -> (Int,Int,Text) 
leftAppendLineNumbers code = (marginWidth, countWidth, (T.unlines . imap go) allLines)
 where 
 go ((+1) -> lineNumber) oneLine = getLeftMargin lineNumber <> oneLine 
 marginWidth = (fromInteger . toInteger . T.length) (getLeftMargin (0::Integer))  -- assumes the length is constant 
 getLeftMargin lineNumber = "[" <> T.pack (padNumber countWidth lineNumber) <> "]"
 countWidth = length (show lineCount)
 lineCount = length allLines 
 allLines = T.lines code

getPythonErrorSpan :: ParseError -> (Int,Int)
getPythonErrorSpan = maybe (1,1) id . go -- TODO default error span 
 where 

 go = \case
  UnexpectedToken (token_span -> theSpan) -> fromSourceSpan theSpan 
  UnexpectedChar _ location -> fromSourceLocation location 
  _ -> Nothing

 fromSourceSpan = \case
  SpanCoLinear{ span_row, span_start_column } -> Just (span_row, span_start_column) 
  SpanMultiLine{ span_start_row, span_start_column } -> Just (span_start_row, span_start_column) 
  SpanPoint{ span_row, span_column } -> Just (span_row, span_column) 
  _ -> Nothing

 fromSourceLocation = \case
  Sloc{ sloc_row, sloc_column } -> Just (sloc_row, sloc_column) 
  _ -> Nothing 

showWords :: [Text] -> String 
showWords = T.unpack . T.intercalate (T.pack " ")

printMessage :: [String] -> IO ()
printMessage = putStrLn . List.intercalate "\n"

index :: (Integral n, Num n) => [a] -> n -> Maybe a
index [] _ = Nothing 
index (x:xs) n
 | n == 0    = Just x
 | n < 0     = Nothing 
 | otherwise = index xs (n-1)

prompt :: String -> IO String
prompt s = do
    putStr s 
    hFlush stdout
    getLine

type Vocab a = [(String, a)]

vocabWith :: (IsString t, Show t, Functor'RHS n t f) => (a->b) -> Vocab a -> RHS n t f b 
vocabWith f = vocab . fmap (fmap f) 

strip :: String -> String 
strip = rstrip . lstrip
 where
 lstrip = dropWhile (`elem` (" \t\n\r"::String))
 rstrip = reverse . lstrip . reverse

{-| inputs milliseconds, outputs microseconds (which can be given to threadDelay).   

>>> milliseconds 10
10000

-}
milliseconds :: Int -> Int
milliseconds = (*1000)

{-| inputs seconds, outputs microseconds (which can be given to threadDelay).   

>>> seconds 1
1000000

-}
seconds :: Int -> Int
seconds = (*1000000) 

{-| 

-}
readCommand aCommand someArguments = do 
 (exitCode, standardInput, standardError) <- readProcessWithExitCode aCommand someArguments "" 
 return (exitCode, lines standardInput, lines standardError)

