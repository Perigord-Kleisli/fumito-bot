module Fumito.Types (
    FumitoState (..),
    Log,
    lastSequenceNum,
    fumito_resume_gateway_url,
    Fumito.Types.identity,
    module Fumito.Types.Gateway,
    module Fumito.Types.Channel,
    module Fumito.Types.Common,
    module Fumito.Types.Exception,
) where

import Control.Lens (makeLenses)
import Di (Level, Message, Path)
import DiPolysemy (Di)
import Fumito.Types.Channel
import Fumito.Types.Common
import Fumito.Types.Exception
import Fumito.Types.Gateway

type Log = Di Level Message Path

data FumitoState = FumitoState
    { _lastSequenceNum :: IORef (Maybe Integer)
    , _identity :: IdentifyStructure
    , _fumito_resume_gateway_url :: Maybe Text
    }
makeLenses ''FumitoState
