module Fumito.Types.Guild where

import Data.Aeson
import Data.Time (UTCTime)
import Fumito.Types.Common (Snowflake, User)

data GuildMember = GuildMember
    { user :: Maybe User
    , nick :: Maybe Text
    , avatar :: Maybe Text
    , roles :: [Snowflake]
    , joined_at :: UTCTime
    , premium_since :: Maybe UTCTime
    , deaf :: Bool
    , mute :: Bool
    , flags :: Word8
    , pending :: Maybe Bool
    , permissions :: Maybe Text
    , communication_disabled_until :: Maybe UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data VoiceState = VoiceState
    { guild_id :: Maybe Snowflake
    , channel_id :: Maybe Snowflake
    , user_id :: Snowflake
    , member :: GuildMember
    , session_id :: Text
    , deaf :: Bool
    , mute :: Bool
    , self_deaf :: Bool
    , self_mute :: Bool
    , self_stream :: Maybe Bool
    , self_video :: Bool
    , supress :: Bool
    , request_to_speak_timestamp :: Maybe UTCTime
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
