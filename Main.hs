import qualified Commands.Plugins.Spiros as Spiros
import System.Environment (getArgs)

main = mainWith =<< getArgs

mainWith = \case
 _ -> do
  Example.rootServe
