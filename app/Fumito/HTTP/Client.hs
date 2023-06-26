module Fumito.HTTP.Client where

import Data.Aeson
import Fumito.Bot.Types (FumitoOpts (..))
import Fumito.Gateway.Types
import Fumito.HTTP.Types
import Fumito.Types.Channel
import Fumito.Types.Common (Snowflake, snowflakeToText)
import Fumito.Types.Message
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Reader qualified as PR
import Polysemy.Req

baseURL :: Url 'Https
baseURL = https "discord.com" /: "api" /: "v10" /: "channels"

data ChannelRequest m a where
    GetChannel :: Snowflake -> ChannelRequest m (JsonResponse Channel)
    GetChannelMessages :: Snowflake -> ChannelRequest m (JsonResponse [Message])
    GetChannelMessage :: Snowflake -> Snowflake -> ChannelRequest m (JsonResponse Message)
    CreateMessage :: Snowflake -> MessageCreateForm -> ChannelRequest m (JsonResponse Message)
    EditMessage :: Snowflake -> Snowflake -> MessageEditForm -> ChannelRequest m (JsonResponse Message)
    DeleteMessage :: Snowflake -> Snowflake -> ChannelRequest m IgnoreResponse
    BulkDeleteMessage :: Snowflake -> [Snowflake] -> ChannelRequest m IgnoreResponse
makeSem ''ChannelRequest

runChannelRequest :: Members '[PR.Reader FumitoOpts, Req] r => Sem (ChannelRequest ': r) a -> Sem r a
runChannelRequest expr = do
    token <- PR.asks (token . Fumito.Bot.Types.identity)
    let tokenHeader :: Option 'Https = header "Authorization" ("Bot " <> encodeUtf8 token)
    ($ expr) $ interpret \case
        (GetChannel chanid) -> do
            req
                GET
                (baseURL /: snowflakeToText chanid)
                NoReqBody
                jsonResponse
                tokenHeader
        (GetChannelMessages chanid) ->
            req
                GET
                (baseURL /: snowflakeToText chanid /: "messages")
                NoReqBody
                jsonResponse
                tokenHeader
        (GetChannelMessage chanid msgid) ->
            req
                GET
                (baseURL /: snowflakeToText chanid /: "messages" /: snowflakeToText msgid)
                NoReqBody
                jsonResponse
                tokenHeader
        (CreateMessage chanid form) ->
            req
                POST
                (baseURL /: snowflakeToText chanid /: "messages")
                (ReqBodyJson form)
                jsonResponse
                tokenHeader
        (EditMessage chanid msgid form) ->
            req
                PATCH
                (baseURL /: snowflakeToText chanid /: "messages" /: snowflakeToText msgid)
                (ReqBodyJson form)
                jsonResponse
                tokenHeader
        (DeleteMessage chanid msgid) ->
            req
                DELETE
                (baseURL /: snowflakeToText chanid /: "messages" /: snowflakeToText msgid)
                NoReqBody
                ignoreResponse
                tokenHeader
        (BulkDeleteMessage chanid msgids) ->
            req
                DELETE
                (baseURL /: snowflakeToText chanid /: "messages" /: "bulk-delete")
                (ReqBodyJson $ object ["messages" .= msgids])
                ignoreResponse
                tokenHeader
