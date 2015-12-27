{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}
-- | (you can read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Windows.QQ where
import Commands.Plugins.Spiros.Extra ((<>)) 
import Commands.Plugins.Spiros.Windows.Types

import           Text.InterpolatedString.Perl6
import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 



getBatchScript :: BatchScriptR Text -> Text 
getBatchScript BatchScriptR{..} = [qc|
:: {__batchFilePath__}

@echo off
setlocal enableextensions enabledelayedexpansion

wmic process where name="natspeak.exe" CALL setpriority "high priority" 

taskkill /IM slui.exe /F 
:: taskkill /IM egui.exe /F 

taskkill /IM ISUSPM.exe /F 
taskkill /IM dnsspserver.exe /F 
:: taskkill /IM dgnuiasvr_x64.exe /F 

echo {__guestDirectory__}
echo {__natlinkDirectory__} 
echo {__natlinkFile__} 

set COMMANDSTIMESTAMP=0
set DIFFERENCE=0

:: synchronize every second, updating the timestamp, and skating if the source file hasn't changed 
FOR /L %%N IN (0,0,1) DO (
 echo( 
 echo( 
 echo( 
 FOR /f "tokens=2 delims==" %%t IN (
    'WMIC DATAFILE WHERE Name^="{__guestEscaped__}" GET LastModified /value ^| find "="'
 ) DO set TIMESTAMP=%%t 
      set TIMESTAMP=!TIMESTAMP:~4,10!
      set /a DIFFERENCE=!TIMESTAMP! - !COMMANDSTIMESTAMP!
      echo( 
      echo CURR is !COMMANDSTIMESTAMP!
      echo PREV is !TIMESTAMP!
      echo DIFF is !DIFFERENCE!
      IF not !DIFFERENCE! equ 0 (
           echo "updating..." 
           set COMMANDSTIMESTAMP=!TIMESTAMP! 
           copy /Y {__guestDirectory__}\\{__natlinkFile__} {__natlinkDirectory__} /a 
           copy /b {__natlinkDirectory__}\\{__natlinkFile__}+,, {__natlinkDirectory__}\\{__natlinkFile__} 
      ) ELSE ( 
           echo "not updating." 
      )
  timeout /nobreak /t 10
)

|] 
 -- beware: in batch, !x! dereferences x at runtime,
 -- while %x% **interpolates** x at parsetime (of the block). 
 -- and %%t only seems to exist within the "outermost block" of the for loop. 
 -- that's why we need: set TIMESTAMP=%%t 
 -- arbitrary arithmetic doesn't work: set TIMESTAMP=!TIMESTAMP:~0,14! ; set /a DIFFERENCE=(!TIMESTAMP! - !COMMANDSTIMESTAMP!)
 -- because numbers must be 32bit. 
 where 
 __guestEscaped__ = T.replace "\\" "\\\\" (__guestDirectory__ <> "\\" <> __natlinkFile__)
  -- because the path in batch is under double quotes

