module Commands.Plugins.Spiros.Correct where 
import Commands.Plugins.Spiros.Extra
import Commands.Plugins.Spiros.Digit 
import Commands.Plugins.Spiros.Phrase.Types (Dictation(..), words2dictation) 
import Commands.Servers.Servant.Types (HypothesesRequest(..)) 

import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 


{-| a user can choose: 

* one of the (ten or fewer) hypotheses they are presented with.   
* an arbitrary sentence. 
it can be rejected if it sounds too different from the recognition being corrected. 

-}
type Correction = Either Digit Dictation

promptCorrection :: HypothesesRequest -> IO Dictation 
promptCorrection hypotheses = do 
 line <- prompt "correction> " 
 case fromCorrection hypotheses =<< parseCorrection line of 
  Nothing -> promptCorrection hypotheses 
  Just d -> return d 

parseCorrection :: String -> Maybe Correction 
parseCorrection s = (Left <$> parseDigit s) <> (Right <$> isDictation s)
 where isDictation = Just . words2dictation      -- TODO 

fromCorrection :: HypothesesRequest -> Correction -> Maybe Dictation 
fromCorrection hypotheses = either
 (fmap (Dictation . fmap T.unpack) . indexHypotheses hypotheses)
 Just 

indexHypotheses :: HypothesesRequest -> Digit -> Maybe [Text] 
indexHypotheses (HypothesesRequest hs) (Digit i) = hs `index` i

