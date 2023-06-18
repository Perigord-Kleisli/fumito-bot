{-# OPTIONS_GHC -Wno-orphans #-}

module Fumito.Types.Gateway where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Scientific
import Data.String.Interpolate
import Fumito.Types.Common
import Relude.Extra (safeToEnum)

instance Default Text where
    def = ""

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

data PayloadSend
    = HeartBeatSend {last_sequence_num :: Maybe Integer}
    | Identify IdentifyStructure
    deriving stock (Show)

instance ToJSON PayloadSend where
    toJSON (Identify iden) = object ["op" .= (2 :: Int), "d" .= iden]
    toJSON (HeartBeatSend last_s) =
        object
            ["op" .= (1 :: Int), "d" .= last_s]

newtype DispatchEvent a = DispatchEvent {fromDispatch :: a}
    deriving stock (Show)
    deriving newtype (FromJSON, ToJSON)

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

-- TODO: add the rest
data PayloadReceive a where
    HelloEvent :: {heartbeat_interval :: Integer} -> PayloadReceive Integer
    HeartBeatAck :: {zombified_value :: Maybe Int} -> PayloadReceive (Maybe Int)
    Dispatch :: FromJSON a => {d :: DispatchEvent a, t :: Text, s :: Integer} -> PayloadReceive (DispatchEvent a)

opShouldBe :: Int -> Object -> Parser ()
opShouldBe n ob = do
    n' <- ob .: "op"
    if n == n'
        then pass
        else fail [i|Unexpected opcode (#{n'}), expected (#{n})|]

withPayload :: (Object -> Parser a) -> Value -> Parser a
withPayload = withObject "Receive Event Payload"

instance FromJSON (PayloadReceive Integer) where
    parseJSON = withPayload \ob -> do
        opShouldBe 10 ob
        ob
            .: "d"
            >>= withObject
                "Hello Event"
                (fmap HelloEvent . (.: "heartbeat_interval"))

instance FromJSON (PayloadReceive (Maybe Int)) where
    parseJSON = withPayload \ob -> do
        ob .: "op" >>= \case
            (1 :: Int) -> ob .: "d"
            11 -> return (HeartBeatAck Nothing)
            n -> fail [i|Unknown opcode in heartbeat loop (#{n})|]

instance FromJSON a => FromJSON (PayloadReceive (DispatchEvent a)) where
    parseJSON = withPayload \ob -> do
        opShouldBe 0 ob
        d <- ob .: "d" >>= parseJSON
        t <- ob .: "t"
        s <- ob .: "s"
        return Dispatch {d, t, s}
