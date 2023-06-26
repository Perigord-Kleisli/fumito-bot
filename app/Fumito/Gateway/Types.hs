module Fumito.Gateway.Types where

import Data.Aeson
import Data.String.Interpolate
import Data.Time (UTCTime)
import Fumito.Types.Channel (Channel)
import Fumito.Types.Common
import Fumito.Types.Guild (GuildMember)
import Fumito.Types.Payload
import Fumito.Utils
import Relude.Extra (safeToEnum)

data ConnectionProperties = ConnnectionProperties
    { os :: Text
    , browser :: Text
    , device :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ActivityType
    = Game
    | Streaming
    | Listening
    | Watching
    | Custom
    | Competing
    deriving stock (Show, Enum, Bounded)
deriveJSONFromEnum ''ActivityType

data Timestamps = TimeStamps
    { start :: Maybe Integer
    , end :: Maybe Integer
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Activity = Activity
    { name :: Text
    , type_ :: ActivityType
    , url :: Maybe Text
    , created_at :: Integer
    , timestamps :: Maybe Timestamps
    , application_id :: Maybe Snowflake
    , details :: Maybe Text
    , state :: Maybe Text
    , emoji :: Maybe Emoji
    , party :: Maybe Party
    , assets :: Maybe Assets
    , secrets :: Maybe Secrets
    , flags :: Maybe Word8
    , buttons :: Maybe [Button]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data UpdatePrescense = UpdatePrescense
    { since :: Maybe Integer
    , activities :: [Activity]
    , status :: Text
    , afk :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data IdentifyStructure = IdentifyStructure
    { token :: Text
    , properties :: ConnectionProperties
    , compress :: Maybe Bool
    , large_treshold :: Maybe Int
    , shard :: Maybe (Int, Int)
    , presence :: Maybe UpdatePrescense
    , intents :: Word32
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data PayloadSend
    = HeartBeatSend {seqNum :: Maybe Int}
    | Identify IdentifyStructure
    deriving stock (Show)

instance ToJSON PayloadSend where
    toJSON (Identify iden) = object ["op" .= (2 :: Int), "d" .= iden]
    toJSON (HeartBeatSend last_s) =
        object
            ["op" .= (1 :: Int), "d" .= last_s]

data ReadyStructure = ReadyStructure
    { v :: Int
    , user :: User
    , guilds :: [UnavailableGuild]
    , session_id :: Text
    , resume_gateway_url :: Text
    , shard :: Maybe (Int, Int)
    , application :: ReadyEventApplication
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | States of members currently in voice channels; lacks the guild_id key
data GuildCreateVoiceState = GuildCreateVoiceState
    { channel_id :: Maybe Snowflake
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

data ClientStatus = ClientStatus
    { desktop :: Maybe Text
    , mobile :: Maybe Text
    , web :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data PrescenseUpdate = PrescenseUpdate
    { user :: User
    , guild_id :: Snowflake
    , status :: Text
    , activities :: [Activity]
    , client_status :: ClientStatus
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data PrivacyLevel = PUBLIC | GUILD_ONLY deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('PUBLIC, 1)] ''PrivacyLevel

data StageInstance = StageInstance
    { id :: Snowflake
    , guild_id :: Snowflake
    , channel_id :: Snowflake
    , topic :: Text
    , privacy_level :: PrivacyLevel
    , discoverable_disabled :: Maybe Bool
    , guild_scheduled_event_id :: Maybe Snowflake
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EventStatus
    = SCHEDULED
    | ACTIVE
    | COMPLETED
    | CANCELLLED
    deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('SCHEDULED, 1)] ''EventStatus

data GuildScheduledEntity
    = STAGE_INSTANCE
    | VOICE
    | EXTERNAL
    deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('STAGE_INSTANCE, 1)] ''GuildScheduledEntity

newtype EntityMetadata = EntityMetadata {location :: Maybe Text}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GuildScheduledEvent = GuildScheduledEvent
    { id :: Snowflake
    , guild_id :: Snowflake
    , channel_id :: Maybe Snowflake
    , creator_id :: Maybe Snowflake
    , name :: Text
    , description :: Maybe Text
    , scheduled_start_time :: UTCTime
    , scheduled_end_time :: Maybe UTCTime
    , privacy_level :: PrivacyLevel
    , status :: EventStatus
    , entity_type :: GuildScheduledEntity
    , entity_id :: Maybe Snowflake
    , entity_metadata :: Maybe EntityMetadata
    , creator :: Maybe User
    , user_count :: Maybe Integer
    , image :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GuildCreateStructure = GuildCreateStructure
    { joined_at :: UTCTime
    , large :: Bool
    , unavailable :: Maybe Bool
    , member_count :: Integer
    , voice_states :: [GuildCreateVoiceState]
    , members :: [GuildMember]
    , channels :: [Channel]
    , threads :: [Channel]
    , presences :: [PrescenseUpdate]
    , stage_instances :: [StageInstance]
    , guild_scheduled_events :: [GuildScheduledEvent]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TypingStartEvent = TypingStartEvent
    { channel_id :: Snowflake
    , guild_id :: Maybe Snowflake
    , user_id :: Snowflake
    , timestamp :: Int
    , member :: Maybe GuildMember
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DispatchEvent
    = READY ReadyStructure
    | GUILD_CREATE GuildCreateStructure
    | TYPING_START TypingStartEvent
    | MESSAGE_CREATE MessageCreatePayload
    | MESSAGE_EDIT MessageEditPayload
    | MESSAGE_DELETE MessageDeletePayload
    | MESSAGE_DELETE_BULK MessageDeleteBulkPayload
    deriving stock (Show, Generic)

-- TODO: add the rest
data PayloadReceive
    = Dispatch {d :: DispatchEvent, s :: Int}
    | HeartbeatReceive {last_sequence_num :: Maybe Integer}
    | HelloEvent {heartbeat_interval :: Int}
    | HeartBeatAck
    deriving stock (Show)

instance FromJSON PayloadReceive where
    parseJSON = withObject "Receive Event Payload" \ob -> do
        ob .: "op" >>= \case
            0 -> do
                Dispatch
                    <$> ( ob .: "t" >>= \case
                            "READY" -> READY <$> ob .: "d"
                            "GUILD_CREATE" -> GUILD_CREATE <$> ob .: "d"
                            "TYPING_START" -> TYPING_START <$> ob .: "d"
                            "MESSAGE_CREATE" -> MESSAGE_CREATE <$> ob .: "d"
                            "MESSAGE_EDIT" -> MESSAGE_EDIT <$> ob .: "d"
                            "MESSAGE_DELETE" -> MESSAGE_DELETE <$> ob .: "d"
                            "MESSAGE_DELETE_BULK" -> MESSAGE_DELETE_BULK <$> ob .: "d"
                            (e :: Text) -> fail [i|Unknown dispatch event '#{e}'|]
                        )
                    <*> ob
                        .: "s"
            1 ->
                HeartbeatReceive
                    <$> ob
                        .: "d"
            10 ->
                (ob .: "d") >>= withObject "Hello Event" (fmap HelloEvent . (.: "heartbeat_interval"))
            11 -> return HeartBeatAck
            (n :: Int) -> fail [i|Unknown OpCode (#{n})|]
