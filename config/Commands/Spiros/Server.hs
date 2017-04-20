module Commands.Spiros.Server where
import Commands.Plugins.Spiros

import Commands.Servers.Simple
-- import Workflow.<platform>
import Commands.Backends.Workflow

{-

'windowsHowToSendText' = 'defaultWindowsHowToSendText'
'windowsStepDelay'     = 'defaultWindowsStepDelay'

-}

main = do
	putStrLn "(commands-spiros-server)"
	runSimpleServer $ defaultSettings (ExecuteWorkflow (runWorkflowWithT defaultWindowsWorkflowConfig))
