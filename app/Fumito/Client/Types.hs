module Fumito.Client.Types where

import Fumito.Gateway.Types

newtype FumitoOpts = FumitoOpts
    { identity :: IdentifyStructure
    }
    deriving stock (Generic)
