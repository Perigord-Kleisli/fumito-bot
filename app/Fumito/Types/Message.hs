module Fumito.Types.Message where

import Data.Aeson
import Data.Aeson.KeyMap (insert)
import Data.Aeson.TH
import Data.Char
import Data.Time
import Fumito.Types.Channel (Channel, ChannelMention, ChannelType)
import Fumito.Types.Common hiding (Button)
import Fumito.Types.Guild (GuildMember)
import Fumito.Utils
import Relude.Extra (safeToEnum)

data Attachment = Attachment
    { id :: Snowflake
    , filename :: Text
    , description :: Maybe Text
    , content_type :: Maybe Text
    , size :: Integer
    , url :: Text
    , proxy_url :: Text
    , height :: Maybe Int
    , width :: Maybe Int
    , ephemeral :: Maybe Bool
    , duration_secs :: Maybe Double
    , waveform :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedType
    = RICH
    | IMAGE
    | VIDEO
    | GIFV
    | ARTICLE
    | LINK
    deriving stock (Show, Eq)
deriveJSON defaultOptions {constructorTagModifier = map toLower} ''EmbedType

data EmbedFooter = EmbedFooter
    { text :: Text
    , icon_url :: Maybe Text
    , proxy_icon_url :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedImage = EmbedImage
    { url :: Text
    , proxy_url :: Maybe Text
    , height :: Maybe Int
    , width :: Maybe Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedThumbnail = EmbedThumbnail
    { url :: Text
    , proxy_url :: Maybe Text
    , height :: Maybe Int
    , width :: Maybe Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedVideo = EmbedVideo
    { url :: Text
    , proxy_url :: Maybe Text
    , height :: Maybe Int
    , width :: Maybe Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedProvider = EmbedProvider
    { name :: Maybe Text
    , url :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedAuthor = EmbedAuthor
    { name :: Text
    , url :: Maybe Text
    , icon_url :: Maybe Text
    , proxy_icon_url :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data EmbedField = EmbedField
    { name :: Text
    , value :: Text
    , inline :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Embed = Embed
    { title :: Maybe Text
    , type_ :: Maybe EmbedType
    , description :: Maybe Text
    , url :: Maybe Text
    , timestamp :: Maybe UTCTime
    , color :: Maybe Integer
    , footer :: Maybe EmbedFooter
    , image :: Maybe EmbedImage
    , thumbnail :: Maybe EmbedThumbnail
    , video :: Maybe EmbedVideo
    , provider :: Maybe EmbedProvider
    , author :: Maybe EmbedAuthor
    , fields :: Maybe [EmbedField]
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "type_" -> "type"; n -> n }} ''Embed

data Reaction = Reaction
    { count :: Int
    , me :: Bool
    , emoji :: Emoji
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MessageType
    = DEFAULT
    | RECIPIENT_ADD
    | RECIPIENT_REMOVE
    | CALL
    | CHANNEL_NAME_CHANGE
    | CHANNEL_ICON_CHANGE
    | CHANNEL_PINNED_MESSAGE
    | USER_JOIN
    | GUILD_BOOST
    | GUILD_BOOST_TIER_1
    | GUILD_BOOST_TIER_2
    | GUILD_BOOST_TIER_3
    | CHANNEL_FOLLOW_ADD
    | GUILD_DISCOVERY_DISQUALIFIED
    | GUILD_DISCOVERY_REQUALIFIED
    | GUILD_DISCOVERY_GRACE_PERIOD_INITIAL_WARNING
    | GUILD_DISCOVERY_GRACE_PERIOD_FINAL_WARNING
    | THREAD_CREATED
    | REPLY
    | CHAT_INPUT_COMMAND
    | THREAD_STARTER_MESSAGE
    | GUILD_INVITE_REMINDER
    | CONTEXT_MENU_COMMAND
    | AUTO_MODERATION_ACTION
    | ROLE_SUBSCRIPTION_PURCHASE
    | INTERACTION_PREMIUM_UPSELL
    | STAGE_START
    | STAGE_END
    | STAGE_SPEAKER
    | STAGE_TOPIC
    | GUILD_APPLICATION_PREMIUM_SUBSCRIPTION
    deriving stock (Show, Enum, Bounded, Eq)
deriveJSONFromEnum ''MessageType

data MessageActivity
    = JOIN
    | SPECTATE
    | LISTEN
    | JOIN_REQUEST
    deriving stock (Show, Bounded, Eq)
deriveGappedJSONEnum [('JOIN, 1), ('JOIN_REQUEST, 5)] ''MessageActivity

data InteractionType
    = PING
    | APPLICATION_COMMAND
    | MESSAGE_COMPONENT
    | APPLICATION_COMMAND_AUTOCOMPLETE
    | MODAL_SUBMIT
    deriving stock (Show, Bounded, Eq)
deriveGappedJSONEnum [('PING, 1)] ''InteractionType

data MessageInteraction = MessageInteraction
    { id :: Snowflake
    , type_ :: InteractionType
    , name :: Text
    , user :: User
    , member :: Maybe GuildMember
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "type_" -> "type"; n -> n }} ''MessageInteraction

data StickerType = STANDARD | GUILD
    deriving stock (Show, Bounded, Eq)
deriveGappedJSONEnum [('STANDARD, 1)] ''StickerType

data StickerFormat
    = PNG
    | APNG
    | LOTTIE
    | GIF
    deriving stock (Show, Bounded, Eq)
deriveGappedJSONEnum [('PNG, 1)] ''StickerFormat

data Sticker = Sticker
    { id :: Snowflake
    , pack_id :: Maybe Snowflake
    , name :: Text
    , description :: Maybe Text
    , tags :: Text
    , asset :: Maybe Text
    , type_ :: StickerType
    , format_type :: StickerFormat
    , available :: Maybe Bool
    , guild_id :: Maybe Snowflake
    , user :: Maybe User
    , sort_value :: Maybe Int
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "type_" -> "type"; n -> n }} ''Sticker

data StickerItem = StickerItem
    { id :: Snowflake
    , name :: Text
    , format_type :: StickerFormat
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RoleSubscriptionData = RoleSubscriptionData
    { role_subscription_listing_id :: Snowflake
    , tier_name :: Text
    , total_months_subscribed :: Int
    , is_renewal :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ButtonStyle
    = BUTTON_PRIMARY
    | BUTTON_SECONDARY
    | BUTTON_SUCCESS
    | BUTTON_DANGER
    | BUTTON_LINK
    deriving stock (Show, Bounded, Eq)
deriveGappedJSONEnum [('BUTTON_PRIMARY, 1)] ''ButtonStyle

data SelectOption = SelectOption
    { label :: Text
    , value :: Text
    , description :: Maybe Text
    , emoji :: Maybe Emoji
    , default_ :: Maybe Bool
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "default_" -> "default"; n -> n }} ''SelectOption

data SelectMenu = SelectMenu
    { custom_id :: Text
    , options :: Maybe [SelectOption]
    , channel_types :: Maybe [ChannelType]
    , placeholder :: Maybe Text
    , min_values :: Maybe Int
    , max_values :: Maybe Int
    , disabled :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TextInputStyle = SHORT | PARAGRAPH deriving stock (Show, Eq, Bounded)
deriveGappedJSONEnum [('SHORT, 1)] ''TextInputStyle

data TextInputStructure = TextInputStructure
    { custom_id :: Text
    , style :: TextInputStyle
    , label :: Text
    , min_length :: Maybe Int
    , max_length :: Maybe Int
    , required :: Maybe Bool
    , value :: Maybe Text
    , placeholder :: Maybe Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ButtonStructure = ButtonStructure
    { style :: ButtonStyle
    , label :: Maybe Text
    , emoji :: Maybe Emoji
    , custom_id :: Maybe Text
    , url :: Maybe Text
    , disable :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MessageComponent
    = ActionRow {components :: [MessageComponent]}
    | Button ButtonStructure
    | StringSelect SelectMenu
    | TextInput TextInputStructure
    | UserSelect SelectMenu
    | RoleSelect SelectMenu
    | MentionableSelect SelectMenu
    | ChannelSelect SelectMenu
    deriving stock (Show)

instance ToJSON MessageComponent where
    toJSON (ActionRow {components}) = object ["type" .= (1 :: Int), "components" .= components]
    toJSON (Button x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 2) ob)
        v -> v
    toJSON (StringSelect x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 3) ob)
        v -> v
    toJSON (TextInput x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 4) ob)
        v -> v
    toJSON (UserSelect x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 5) ob)
        v -> v
    toJSON (RoleSelect x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 6) ob)
        v -> v
    toJSON (MentionableSelect x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 7) ob)
        v -> v
    toJSON (ChannelSelect x) = case toJSON x of
        Object ob -> Object (insert "type" (Number 8) ob)
        v -> v

instance FromJSON MessageComponent where
    parseJSON = withObject "MessageComponent" \ob -> do
        ob .: "type" >>= \case
            1 -> ActionRow <$> ob .: "components"
            2 -> Button <$> parseJSON (Object ob)
            3 -> StringSelect <$> parseJSON (Object ob)
            4 -> TextInput <$> parseJSON (Object ob)
            5 -> UserSelect <$> parseJSON (Object ob)
            6 -> RoleSelect <$> parseJSON (Object ob)
            7 -> MentionableSelect <$> parseJSON (Object ob)
            8 -> ChannelSelect <$> parseJSON (Object ob)
            (n :: Int) -> fail $ "Unknown MessageComponent type: " ++ show n

data Message = Message
    { id :: Snowflake
    , channel_id :: Snowflake
    , author :: User
    , content :: Text
    , timestamp :: UTCTime
    , edited_timestamp :: Maybe UTCTime
    , tts :: Bool
    , mention_everyone :: Bool
    , mentions :: [User]
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
    }
    deriving stock (Show)
deriveJSON defaultOptions {fieldLabelModifier = \case { "type_" -> "type"; n -> n }} ''Message
