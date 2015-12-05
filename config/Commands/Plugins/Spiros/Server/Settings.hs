{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module Commands.Plugins.Spiros.Server.Settings where 
import Commands.Servers.Servant.V 
import           Data.Address 
import qualified           Commands.Frontends.Dragon13 as Dragon 

import           Control.Lens 


spirosSettings_ = VSettings_{..} 
 where 
 vPort = Port 8888     -- spirosPort = spirosSettings_ ^. (to(vPort) . _Port)
 vUIAddress = Address localhost (Port 8889)
 vNatLinkSettings = Dragon.NatLinkConfig -- TODO 
  (Address (Host "192.168.56.1") vPort)
  "E:/commands/log.txt"
  "E:/commands/context.json"

spirosDnsOptimizationSettings = Dragon.defaultDnsOptimizationSettings
 & Dragon.dnsOptimizeInlineSmall .~ True
 -- & .~ 

