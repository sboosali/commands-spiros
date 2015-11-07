{-# LANGUAGE ViewPatterns, LambdaCase #-} 
{-| "Commands.Plugins.Spiros.Root" 'Commands.Plugins.Spiros.Root.root' 

-}
module Commands.Plugins.Spiros.Correct where 
import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Digit 
import Commands.Plugins.Spiros.Phrase.Types (Dictation(..), words2dictation)

import Commands.Servers.Servant.Types (HypothesesRequest(..)) 
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 
import Data.Monoid              (First(..))


{-| a user can choose: 

* one of the (ten or fewer) hypotheses they are presented with.   
* an arbitrary sentence. 
it can be rejected if it sounds too different from the recognition being corrected. 

-}
type Correction = Either Digit Dictation

promptCorrection :: HypothesesRequest -> IO Dictation 
promptCorrection hypotheses = do 
 line <- prompt "correction> " 
 case getCorrection hypotheses line of 
  Nothing -> promptCorrection hypotheses 
  Just d -> return d 

{-| 

>>> getCorrection (HypothesesRequest [["hello", "world"]]) "0" 
Just (Dictation ["hello","world"]) 
>>> getCorrection (HypothesesRequest [["hello", "world"]]) "1"
Nothing   
>>> getCorrection (HypothesesRequest [["hello", "world"]]) "some words"  
Just (Dictation ["some","words"]) 

-}
getCorrection :: HypothesesRequest -> String -> Maybe Dictation 
getCorrection hypotheses line = fromCorrection hypotheses =<< parseCorrection line

parseCorrection :: String -> Maybe Correction 
parseCorrection (strip -> s) = getFirst $ First (Left <$> parseDigit s) <> First (Right <$> isDictation s)
 where isDictation = fmap (words2dictation . NonEmpty.toList) . NonEmpty.nonEmpty      -- TODO 

fromCorrection :: HypothesesRequest -> Correction -> Maybe Dictation 
fromCorrection hypotheses = either
 (fmap (Dictation . fmap T.unpack) . indexHypotheses hypotheses)
 Just 

indexHypotheses :: HypothesesRequest -> Digit -> Maybe [Text] 
indexHypotheses (HypothesesRequest hs) (Digit i) = hs `index` i

-- concurrency, other requests shouldn't be buffered, they should be ignored. 
-- consistency between the server state and the client state. 
-- zero-based, original recognition in parentheses, out of order, OR decrement index no don't 
-- haskell flush clear stdin  

