module Fumito.Types.Channel where

import Data.Aeson

data GuildMember = GuildMember
    { memberNick :: Maybe Text
    , memberAvatar :: Maybe Text
    , memberDeaf :: Bool
    , memberMute :: Bool
    , memberPending :: Bool
    , memberPermissions :: Maybe Text
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)
