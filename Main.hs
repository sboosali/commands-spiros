{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import qualified Commands.Plugins.Spiros.Main as Main 
import System.Environment (getArgs)

main = Main.mainWith =<< getArgs

