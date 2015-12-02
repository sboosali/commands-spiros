{-# LANGUAGE QuasiQuotes, DeriveFunctor, RecordWildCards #-}
-- | (you shouldcan read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Windows.QQ where
import Commands.Plugins.Spiros.Extra (Generic, Data, CanInterpolate) 

import           Text.InterpolatedString.Perl6


{-| 

-}
data BatchScriptR t = BatchScriptR 
 { __sourceDirectory__      :: t 
 , __destinationDirectory__ :: t 
 , __shimFile__             :: t 
 -- , :: t 
 } deriving (Show,Eq,Ord,Functor,Data,Generic)

getBatchScript :: (CanInterpolate t) => BatchScriptR t -> t
getBatchScript BatchScriptR{..} = [qc|

wmic process where name="natspeak.exe" CALL setpriority "high priority" 

taskkill /IM slui.exe /F 
:: taskkill /IM egui.exe /F 

taskkill /IM ISUSPM.exe /F 
taskkill /IM dnsspserver.exe /F 
:: taskkill /IM dgnuiasvr_x64.exe /F 

SET SOURCE={__sourceDirectory__} 
SET DESTINATION={__destinationDirectory__} 
SET FILE={__shimFile__} 

ECHO %SOURCE% 
ECHO %DESTINATION% 
ECHO %FILE% 

:: synchronize every second, updating the timestamp  
@FOR /L %N IN (0,0,1) DO @(
 @timeout /nobreak /t 1
 @copy /Y %SOURCE%\%FILE% %DESTINATION% /a 
 @copy /b %DESTINATION%\%FILE%+,, %DESTINATION%\%FILE%
 @FOR %a IN (%DESTINATION%\%FILE%) DO set FileDate=%~ta
 @echo %FileDate% 
)

|] 
