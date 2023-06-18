module Fumito.Types (
    FumitoState (..),
    lastSequenceNum,
    fumito_resume_gateway_url,
    Fumito.Types.identity,
    module Fumito.Types.Gateway,
    module Fumito.Types.Channel,
    module Fumito.Types.Common,
    module Fumito.Types.Exception,
) where

import Control.Lens (makeLenses)
import Data.Default (Default (..))
import Fumito.Types.Channel
import Fumito.Types.Common
import Fumito.Types.Exception
import Fumito.Types.Gateway

data FumitoState = FumitoState
    { _lastSequenceNum :: Maybe Integer
    , _identity :: IdentifyStructure
    , _fumito_resume_gateway_url :: Maybe Text
    }
    deriving stock (Show)
makeLenses ''FumitoState

instance Default FumitoState where
    def =
        FumitoState
            { _lastSequenceNum = Nothing
            , _identity = def
            , _fumito_resume_gateway_url = Nothing
            }
