{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures  #-}
module Commands.Plugins.Spiros.Windows
 ( module Commands.Plugins.Spiros.Windows
 , module Commands.Plugins.Spiros.Windows.QQ
 ) where

import Commands.Plugins.Spiros.Windows.QQ


myBatchScript = getBatchScript myBatchScriptR :: String 

myBatchScriptR = BatchScriptR{..}
 where 
 __sourceDirectory__      = "E:" 
 __destinationDirectory__ = "C:\\NatLink\\NatLink\\MacroSystem\\" 
 __shimFile__             = "_commands.py" 

