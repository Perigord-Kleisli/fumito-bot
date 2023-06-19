module Fumito.Types.Channel where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import Fumito.Types.Common
import Fumito.Types.Guild (GuildMember)
import Fumito.Utils
import Relude.Extra (safeToEnum)

data ChannelType
    = GUILD_TEXT
    | DM
    | GUILD_VOICE
    | GROUP_DM
    | GUILD_CATEGORY
    | GUILD_ANNOUNCEMENT
    | ANNOUNCEMENT_THREAD
    | PUBLIC_THREAD
    | PRIVATE_THREAD
    | GUILD_STAGE_VOICE
    | GUILD_DIRECTORY
    | GUILD_FORUM
    deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('ANNOUNCEMENT_THREAD, 10)] ''ChannelType

data OverwriteType = Role | Member deriving stock (Show, Enum, Bounded)
deriveJSONFromEnum ''OverwriteType

data Overwrite = Overwrite
    { id :: Snowflake
    , type_ :: Text
    , allow :: Int
    , deny :: Int
    }
    deriving stock (Show, Generic)
deriveJSON
    defaultOptions {fieldLabelModifier = \case "type_" -> "type"; n -> n}
    ''Overwrite

data VideoQualityMode
    = AUTO
    | FULL
    deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('AUTO, 1)] ''VideoQualityMode

data ThreadMetaData = ThreadMetaData
    { archived :: Bool
    , auto_archive_duration :: Int
    , archive_timestamp :: UTCTime
    , locked :: Bool
    , invitable :: Maybe Bool
    , create_timestamp :: Maybe UTCTime
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ThreadMember = ThreadMember
    { id :: Maybe Snowflake
    , user_id :: Maybe Snowflake
    , join_timestamp :: UTCTime
    , flags :: Word32
    , member :: Maybe GuildMember
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ForumTag = ForumTag
    { id :: Snowflake
    , name :: Text
    , moderated :: Bool
    , emoji_id :: Maybe Snowflake
    , emoji_name :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data DefaultReaction = DefaultReaction
    { emoji_id :: Maybe Snowflake
    , emoji_name :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ForumSortOrder = LATEST_ACTIVITY | CREATION_DATE
    deriving stock (Enum, Show, Bounded)
deriveJSONFromEnum ''ForumSortOrder

data ForumLayout
    = NOT_SET
    | LIST_VIEW
    | GALLERY_VIEW
    deriving stock (Enum, Show, Bounded)
deriveJSONFromEnum ''ForumLayout

data Channel = Channel
    { id :: Snowflake
    , type_ :: ChannelType
    , guild_id :: Maybe Snowflake
    , position :: Maybe Int
    , permission_overwrites :: [Overwrite]
    , name :: Maybe Text
    , topic :: Maybe Text
    , nsfw :: Maybe Bool
    , last_message_id :: Maybe Snowflake
    , bitrate :: Maybe Integer
    , user_limit :: Maybe Int
    , rate_limit_per_user :: Maybe Int
    , recipients :: Maybe [User]
    , icon :: Maybe Text
    , owner_id :: Maybe Snowflake
    , application_id :: Maybe Snowflake
    , managed :: Maybe Bool
    , parent_id :: Maybe Snowflake
    , last_pin_timestamp :: Maybe UTCTime
    , rtc_region :: Maybe Text
    , video_quality_mode :: Maybe VideoQualityMode
    , message_count :: Maybe Integer
    , member_count :: Maybe Int
    , thread_metadata :: Maybe ThreadMetaData
    , member :: Maybe ThreadMember
    , default_auto_archive_duration :: Maybe Int
    , permissions :: Maybe Text
    , flags :: Maybe Word8
    , total_message_sent :: Maybe Integer
    , available_tags :: Maybe [ForumTag]
    , applied_tags :: Maybe [Snowflake]
    , default_reaction_emoji :: Maybe DefaultReaction
    , default_thread_rate_limit_per_user :: Maybe Int
    , default_sort_order :: Maybe ForumSortOrder
    , default_forum_layout :: Maybe ForumLayout
    }
    deriving stock (Show, Generic)

deriveJSON
    defaultOptions {fieldLabelModifier = \case "type_" -> "type"; n -> n}
    ''Channel
