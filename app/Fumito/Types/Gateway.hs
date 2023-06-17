{-# OPTIONS_GHC -Wno-orphans #-}

module Fumito.Types.Gateway where

import Data.Aeson
import Data.Default
import Data.Scientific
import Fumito.Types.Common
import GHC.Base (errorWithoutStackTrace)
import Relude.Extra (safeToEnum)

instance Default Text where
    def = ""

data Opcode
    = Dispatch
    | Heartbeat
    | Identify
    | PresenceUpdate
    | VoiceStateUpdate
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatAck
    deriving stock (Show, Eq, Bounded)

instance Default Opcode where
    def = Dispatch

instance Enum Opcode where
    fromEnum Dispatch = 0
    fromEnum Heartbeat = 1
    fromEnum Identify = 2
    fromEnum PresenceUpdate = 3
    fromEnum VoiceStateUpdate = 4
    fromEnum Resume = 6
    fromEnum Reconnect = 7
    fromEnum RequestGuildMembers = 8
    fromEnum InvalidSession = 9
    fromEnum Hello = 10
    fromEnum HeartbeatAck = 11

    toEnum 0 = Dispatch
    toEnum 1 = Heartbeat
    toEnum 2 = Identify
    toEnum 3 = PresenceUpdate
    toEnum 4 = VoiceStateUpdate
    toEnum 6 = Resume
    toEnum 7 = Reconnect
    toEnum 8 = RequestGuildMembers
    toEnum 9 = InvalidSession
    toEnum 10 = Hello
    toEnum 11 = HeartbeatAck
    toEnum _ = errorWithoutStackTrace "Fumito.Types.Gateway.toEnum bad argument"

instance FromJSON Opcode where
    parseJSON = withScientific "Gateway Opcode" \n -> do
        when (isFloating n) (fail "Expected Integer Value")
        maybe (fail ("Unknown opcode: " ++ show n)) pure $ safeToEnum $ floor n
instance ToJSON Opcode where
    toJSON = Number . fromIntegral . fromEnum

newtype HelloEvent = HelloEvent {heartbeat_interval :: Integer}
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ConnectionProperties = ConnnectionProperties
    { os :: Text
    , browser :: Text
    , device :: Text
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON, Default)

data ActivityType
    = Game
    | Streaming
    | Listening
    | Watching
    | Custom
    | Competing
    deriving stock (Show, Enum, Bounded)

instance ToJSON ActivityType where
    toJSON = Number . fromIntegral . fromEnum

instance FromJSON ActivityType where
    parseJSON = withScientific "Activity Type" \n -> do
        when (isFloating n) do fail "Expected Integer Value"
        maybe (fail ("Unknown opcode: " ++ show n)) pure $ safeToEnum $ floor n

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
    deriving anyclass (FromJSON, ToJSON, Default)

data GatewayEventPayload a = Payload
    { op :: Opcode
    , d :: a
    , s :: Maybe Integer
    , t :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Default)
