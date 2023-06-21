module Fumito.Gateway.Shard where

data ShardState = ShardState
    { heartBeatAckResponse :: Bool
    , wsResumeURL :: Maybe Text
    , seqNum :: Maybe Int
    }
    deriving stock (Generic)
