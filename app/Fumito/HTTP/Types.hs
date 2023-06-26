module Fumito.HTTP.Types (
    Request (..),
    JSONRequest (..),
    MessageCreateForm (..),
    MessageEditForm (..),
) where

import Control.Lens (ix, (%~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (Options (constructorTagModifier), defaultOptions, deriveJSON)
import Data.Char (toLower)
import Fumito.Types.Common
import Fumito.Types.Message (Embed, MessageComponent)
import Polysemy.Req

class Request a where
    request :: a -> JSONRequest

data JSONRequest where
    Delete :: Url 'Https -> Option 'Https -> JSONRequest
    Get :: Url 'Https -> Option 'Https -> JSONRequest
    Put :: HttpBody a => Url 'Https -> a -> Option 'Https -> JSONRequest
    Patch :: HttpBody a => Url 'Https -> a -> Option 'Https -> JSONRequest
    Post :: HttpBody a => Url 'Https -> a -> Option 'Https -> JSONRequest

data MentionType = Roles | Users | Everyone deriving stock (Show, Eq, Enum)
deriveJSON defaultOptions {constructorTagModifier = ix 0 %~ Data.Char.toLower} ''MentionType

data AllowedMention = AllowedMention
    { parse :: [MentionType]
    , roles :: [Snowflake]
    , users :: [Snowflake]
    , replied_user :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MessageReference = MessageReference
    { messsage_id :: Maybe Snowflake
    , channel_id :: Maybe Snowflake
    , guild_id :: Maybe Snowflake
    , fail_if_not_exists :: Maybe Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MessageCreateForm = MessageCreateForm
    { content :: Maybe Text
    , nonce :: Maybe Nonce
    , tts :: Bool
    , embeds :: Maybe [Embed]
    , allowed_mentions :: Maybe AllowedMention
    , message_reference :: Maybe MessageReference
    , components :: Maybe [MessageComponent]
    , sticker_ids :: Maybe [Snowflake]
    , flags :: Word8
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MessageEditForm = MessageEditForm
    { content :: Text
    , embeds :: Maybe [Embed]
    , flags :: Word8
    , allowed_mentions :: Maybe AllowedMention
    , components :: Maybe [MessageComponent]
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
