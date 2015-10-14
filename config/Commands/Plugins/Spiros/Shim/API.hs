{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable,DeriveTraversable #-}
{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, DataKinds, TypeOperators  #-}
{-| 

-}
module Commands.Plugins.Spiros.Shim.API where

import Commands.Etc
import Commands.Servers.Servant.Types

import Servant
import Data.Aeson (FromJSON) 


{-| 

-}
type CorrectAPI
    = "correct" :> ReqBody '[JSON] DNSHypotheses :> Post '[JSON] ()
  -- :<|> 

-- data DNSCorrection = DNSCorrection 

{-| the hypotheses of the previous recognition. 

its length is between one and ten. 

-}
data DNSHypotheses = DNSHypotheses [DGNRecognition] 
 deriving (Show,Read,Eq,Ord,Data,Generic,FromJSON)

