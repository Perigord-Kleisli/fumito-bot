module Fumito.Types.Common where

import Data.Aeson
import Data.Bits
import Data.Scientific (isFloating)
import Relude.Extra (safeToEnum)

newtype Snowflake = Snowflake Word64
    deriving newtype (Ord, Eq, Num, Integral, Enum, Real, Bits, Read, Show)

instance ToJSON Snowflake where
    toJSON = String . show

instance FromJSON Snowflake where
    parseJSON =
        withText "Snowflake" $
            maybe (fail "Invalid Snowflake") pure
                . readMaybe @Snowflake
                . toString

data Emoji = Emoji
    { name :: Text
    , id :: Maybe Snowflake
    , animated :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
