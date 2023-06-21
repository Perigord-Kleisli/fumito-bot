module Fumito.Client.Types where

import Di qualified as D
import DiPolysemy (Di)
import Fumito.Gateway.Types

type Log = Di D.Level D.Message D.Path

data FumitoState = FumitoState
    { lastSequenceNum :: IORef (Maybe Integer)
    , identity :: IdentifyStructure
    , fumito_resume_gateway_url :: Maybe Text
    }
    deriving stock (Generic)
