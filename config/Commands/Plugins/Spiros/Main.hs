{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Main where 
import Commands.Plugins.Spiros.Server 
import Commands.Plugins.Spiros.Finite 

import System.Environment (getArgs)

main = mainWith =<< getArgs

mainWith = \case

 [] -> spirosServer

 ["test"] -> spirosTest

 ["finite"] -> printFinite

 ["derive"] -> printClick

 _ -> return() 
