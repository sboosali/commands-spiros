{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Commands.Plugins.Spiros.Windows
 ( module Commands.Plugins.Spiros.Windows
 , module Commands.Plugins.Spiros.Windows.Types 
 , module Commands.Plugins.Spiros.Windows.QQ
 ) where

import Commands.Plugins.Spiros.Windows.Types
import Commands.Plugins.Spiros.Windows.QQ

import System.FilePath ((</>))


writeBatchScript :: BatchScriptR String -> IO ()
writeBatchScript s = writeFile (getBatchScriptPath s) (getBatchScript s)

getBatchScriptPath :: BatchScriptR FilePath -> FilePath 
-- getBatchScriptPath :: (CanInterpolate t) => BatchScriptR t -> t 
getBatchScriptPath BatchScriptR{..} = (__hostDirectory__ </> __natlinkFile__)


myBatchScriptR :: BatchScriptR String 
myBatchScriptR = BatchScriptR{..} -- TODO separate library from configuration {-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

 where 
 __hostDirectory__    = "/Users/sboosalis/Desktop" 
 __guestDirectory__   = "E:\\" 
 __natlinkDirectory__ = "C:\\NatLink\\NatLink\\MacroSystem\\" 
 __natlinkFile__      = "_commands.py" 

