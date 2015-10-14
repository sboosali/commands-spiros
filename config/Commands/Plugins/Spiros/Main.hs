{-# LANGUAGE LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
module Commands.Plugins.Spiros.Main where 
import Commands.Plugins.Spiros.Server 


mainWith :: [String] -> IO ()
mainWith = \case

 [] -> do
  spirosServer

 ["test"] -> do
  spirosTest

 _ -> do
  return() 

