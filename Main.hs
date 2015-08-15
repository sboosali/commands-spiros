{-# LANGUAGE LambdaCase #-} 
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Commands.Plugins.Spiros as Spiros
import System.Environment (getArgs)

main = mainWith =<< getArgs

mainWith = \case
 _ -> do
  Spiros.spirosServer
