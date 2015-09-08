{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Commands.Plugins.Spiros.Etc where

-- import           Commands.Mixins.DNS13OSX9
import           Commands.Backends.OSX


nothing :: (Monad m) => m ()
nothing = return ()

slot s = do
 delay 10
 sendText s
 sendKeyPress [] ReturnKey

