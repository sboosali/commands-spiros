{-# LANGUAGE LambdaCase #-} 
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Commands.Plugins.Spiros.Main as Main 
import System.Environment (getArgs)

main = mainWith =<< getArgs

mainWith = \case
 _ -> do
  Main.spirosServer
