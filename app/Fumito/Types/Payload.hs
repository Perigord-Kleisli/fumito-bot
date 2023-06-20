module Fumito.Types.Payload where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.Time (UTCTime)
import Fumito.Types.Channel (Channel, ChannelMention)
import Fumito.Types.Common (Application, PremiumType, Role, Snowflake, User, Nonce)
import Fumito.Types.Guild (GuildMember)
import Fumito.Types.Message (Attachment, Embed, Message, MessageActivity, MessageComponent, MessageInteraction, MessageType, Reaction, RoleSubscriptionData, Sticker, StickerItem)

data MessageCreateUser = MessageCreateUser
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

data MessageCreatePayload = MessageCreatePayload
    { id :: Snowflake
    , channel_id :: Snowflake
    , author :: User
    , content :: Text
    , timestamp :: UTCTime
    , edited_timestamp :: Maybe UTCTime
    , tts :: Bool
    , mention_everyone :: Bool
    , mentions :: [MessageCreateUser]
    , mention_roles :: [Role]
    , mention_channels :: Maybe [ChannelMention]
    , attachments :: [Attachment]
    , embeds :: [Embed]
    , reactions :: Maybe [Reaction]
    , nonce :: Maybe Nonce
    , pinned :: Bool
    , webhook_id :: Maybe Snowflake
    , type_ :: MessageType
    , activity :: Maybe MessageActivity
    , application :: Maybe Application
    , application_id :: Maybe Snowflake
    , message_reference :: Maybe Message
    , flags :: Maybe Word16
    , referenced_message :: Maybe Message
    , interaction :: Maybe MessageInteraction
    , thread :: Maybe Channel
    , components :: Maybe [MessageComponent]
    , sticker_items :: Maybe [StickerItem]
    , stickers :: Maybe [Sticker]
    , position :: Maybe Int
    , role_subscription_data :: Maybe RoleSubscriptionData
    , guild_id :: Maybe Snowflake
    , member :: Maybe GuildMember
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "type_" -> "type"; n -> n }} ''MessageCreatePayload
