{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Spiros.Etc where

-- import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX


nothing :: (Monad m) => m ()
nothing = return ()

slot :: String -> AMonadAction_
slot s = do
 delay 10
 sendText s
 sendKeyPress [] ReturnKey

isDefaultBrowser :: AMonadAction (Maybe String)
isDefaultBrowser = currentApplication >>= \case
 x@"Google Chrome" -> return$ Just x
 _                 -> return$ Nothing 

