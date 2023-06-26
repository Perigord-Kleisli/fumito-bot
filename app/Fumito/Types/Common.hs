module Fumito.Types.Common where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits
import Data.Scientific (isFloating)
import Di qualified as D
import DiPolysemy qualified as DP
import Relude.Extra (safeToEnum)

data Nonce
    = NonceText Text
    | NonceNum Int
    deriving stock (Show)

instance ToJSON Nonce where
    toJSON (NonceText x) = toJSON x
    toJSON (NonceNum x) = toJSON x

instance FromJSON Nonce where
    parseJSON v =
        (NonceText <$> parseJSON v)
            <|> (NonceNum <$> parseJSON v)

newtype Snowflake = Snowflake {unSnowflake :: Word64}
    deriving newtype (Ord, Eq, Num, Integral, Enum, Real, Bits, Read, Show)

snowflakeToText :: Snowflake -> Text
snowflakeToText (Snowflake iden) = show iden

instance ToJSON Snowflake where
    toJSON = String . show

instance FromJSON Snowflake where
    parseJSON =
        withText "Snowflake" $
            maybe (fail "Invalid Snowflake") pure
                . readMaybe @Snowflake
                . toString

data Party = Party
    { id :: Text
    , size :: (Int, Int)
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Assets = Assets
    { large_image :: Maybe Text
    , large_text :: Maybe Text
    , small_image :: Maybe Text
    , small_text :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Secrets = Secrets
    { join :: Text
    , spectate :: Text
    , match :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Button = Button
    { label :: Text
    , url :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PremiumType
    = None
    | NitroClassic
    | Nitro
    | NitroBasic
    deriving stock (Show, Eq, Enum, Bounded)

instance ToJSON PremiumType where
    toJSON = Number . fromIntegral . fromEnum

instance FromJSON PremiumType where
    parseJSON = withScientific "Premium Type" \n -> do
        when (isFloating n) do
            fail "Expected Integer"
        maybe (fail $ "Unknown Premium Type: " ++ show n) pure $ safeToEnum $ floor n

data UnavailableGuild = UnavailableGuild
    {id :: Snowflake, unavailable :: Bool}
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- //TODO Complete Guild object
data Guild = Guild
    { id :: Snowflake
    , name :: Text
    , icon :: Maybe Text
    , splash :: Maybe Text
    , discovery_splash :: Maybe Text
    , emojis :: [Emoji]
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MemberShipState = Invited | Accepted deriving stock (Show)

instance ToJSON MemberShipState where
    toJSON Invited = Number 1
    toJSON Accepted = Number 2

instance FromJSON MemberShipState where
    parseJSON = withScientific "Team Membership State" \n -> do
        when (isFloating n) do fail ""
        case n of
            1 -> return Invited
            2 -> return Accepted
            _ -> fail $ "Unknown Team Membership State: " ++ show n

data TeamMember = TeamMember
    { membership_state :: MemberShipState
    , permissions :: [Text]
    , team_id :: Snowflake
    , user :: User
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Team = Team
    { icon :: Maybe Text
    , id :: Snowflake
    , members :: [TeamMember]
    , name :: Text
    , owner_user_id :: Snowflake
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data InstallParams = InstallParams
    {scopes :: [Text], permissions :: Text}
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ReadyEventApplication = ReadyEventApplication
    { id :: Snowflake
    , flags :: Word32
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Application = Application
    { id :: Snowflake
    , name :: Text
    , icon :: Maybe Text
    , description :: Maybe Text
    , rpc_origins :: Maybe [Text]
    , bot_public :: Bool
    , bot_require_public_grant :: Bool
    , terms_of_service_url :: Maybe Text
    , privacy_policy_url :: Maybe Text
    , owner :: User
    , verify_key :: Text
    , team :: Maybe Team
    , guild_id :: Maybe Snowflake
    , primary_sku_id :: Maybe Snowflake
    , slug :: Maybe String
    , cover_image :: Maybe String
    , flags :: Maybe Word32
    , tags :: Maybe [Text]
    , install_params :: Maybe InstallParams
    , custom_install_url :: Text
    , role_connections_verification_url :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data User = User
    { id :: Snowflake
    , username :: Text
    , discriminator :: Text
    , global_name :: Maybe Text
    , avatar :: Maybe Text
    , bot :: Maybe Bool
    , mfa_enabled :: Maybe Bool
    , banner :: Maybe Text
    , accent_color :: Maybe Int
    , locale :: Maybe Text
    , verified :: Maybe Bool
    , flags :: Maybe Word32
    , premium_type :: Maybe PremiumType
    , public_flags :: Maybe Word32
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RoleTags = RoleTags
    { bot_id :: Maybe Snowflake
    , integration_id :: Maybe Snowflake
    , premium_subscriber :: Bool
    , subscription_listing_id :: Maybe Snowflake
    , available_for_purchase :: Bool
    , guild_connections :: Bool
    }
    deriving stock (Show)

instance ToJSON RoleTags where
    toJSON
        ( RoleTags
                { bot_id
                , integration_id
                , premium_subscriber
                , subscription_listing_id
                , available_for_purchase
                , guild_connections
                }
            ) =
            object $
                [ "bot_id" .= bot_id
                , "integration_id" .= integration_id
                , "subscription_listing_id" .= subscription_listing_id
                ]
                    ++ (["premium_subscriber" .= Null | premium_subscriber])
                    ++ (["available_for_purchase" .= Null | available_for_purchase])
                    ++ (["guild_connections" .= Null | guild_connections])

instance FromJSON RoleTags where
    parseJSON = withObject "RoleTags" \ob ->
        do
            RoleTags
            <$> ob
            .: "bot_id"
            <*> ob
            .: "integration_id"
            <*> fmap isJust (ob .: "premium_subscriber" :: Parser (Maybe ()))
            <*> ob
            .: "subscription_listing_id"
            <*> fmap isJust (ob .: "available_for_purchase" :: Parser (Maybe ()))
            <*> fmap isJust (ob .: "guild_connections" :: Parser (Maybe ()))

data Role = Role
    { id :: Snowflake
    , name :: Text
    , color :: Integer
    , hoist :: Bool
    , icon :: Maybe Text
    , unicode_emoji :: Maybe Text
    , position :: Int
    , permissions :: Text
    , managed :: Bool
    , mentionable :: Bool
    , tags :: Maybe RoleTags
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Emoji = Emoji
    { name :: Text
    , id :: Maybe Snowflake
    , animated :: Maybe Bool
    , roles :: Maybe [Role]
    , user :: Maybe User
    , require_colon :: Maybe Bool
    , managed :: Maybe Bool
    , available :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type LogEff = DP.Di D.Level D.Path D.Message
