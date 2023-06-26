{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Prelude hiding (Reader, runReader)

import Network.WebSockets (defaultConnectionOptions)
import Wuss

import Di (new)

import Data.ByteString qualified as BS

import DiPolysemy
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Websocket

import Control.Exception (throwIO)

import Fumito.Bot.Types
import Fumito.Gateway.Shard (
    GatewayEff,
    closeGateway,
    receiveHelloEvent,
    receivePayload,
    runGateway,
    sendIdentity,
 )
import Fumito.Gateway.Types (
    ConnectionProperties (..),
    IdentifyStructure (..),
 )
import Fumito.HTTP.Client
import Fumito.HTTP.Types
import Polysemy.Reader (runReader)
import Polysemy.Req (interpretReq, responseBody)
import Shower
import System.Console.Haskeline

import Control.Concurrent
import Control.Monad.State qualified as ST
import Data.String.Interpolate

import Fumito.Types.Common (Snowflake (Snowflake))
import Text.Parsec hiding (try, (<|>))
import Text.Parsec.Token (GenLanguageDef (..), GenTokenParser (..), makeTokenParser)

bot :: forall r. Members (ChannelRequest ': GatewayEff) r => Sem r ()
bot = push "gateway" do
    notice @Text "Established connection with gateway"
    receiveHelloEvent

    sendIdentity >>= embed . printer

    replChan <- embed newChan

    replLoop <- async $ infinitely do
        join $ embed $ readChan replChan

    putStrLn "Input initial channel id"
    chan_id <- either Prelude.error id . readEither . toString <$> getLine
    embed $ flip evalStateT (Snowflake chan_id) $ runInputT defaultSettings (loop replChan)
    cancel replLoop
    notice @Text "Closing Gateway Connection"
    closeGateway ("Disconnected" :: Text)
    where
        helpText =
            [__i|
            Fumito Commands
            --------------------------------------------------------
            help                          --   get help message
            quit|:q                       --   close bot
            getEvent                      --   get an event and print it
            channel <channel_id>          --   switch channel
            send <string>                 --   send message in channel
            edit <message_id> <string>    --   edit message in channel
            delete <message_id>           --   delete message in channel
            |]
        loop :: Chan (Sem r ()) -> InputT (StateT Snowflake IO) ()
        loop chan = do
            input <- getInputLine "fumito> "
            whenJust input \input' -> case runParser repl () "Fumito REPL" input' of
                Left _ -> do
                    outputStrLn [i|Unknown input "#{input'}"|]
                    outputStrLn helpText
                    loop chan
                Right x -> x
            where
                TokenParser {reserved, integer, lexeme} =
                    makeTokenParser
                        LanguageDef
                            { reservedNames =
                                ["quit", "help", "getEvent", "channel", "send"]
                                    <> ["edit", "delete"]
                            , reservedOpNames = []
                            , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                            , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                            , nestedComments = True
                            , identStart = letter <|> char '_'
                            , identLetter = alphaNum <|> char '_'
                            , commentStart = "/*"
                            , commentLine = "#"
                            , commentEnd = "*/"
                            , caseSensitive = False
                            }
                snowflakeP = Snowflake . fromInteger <$> integer
                repl =
                    choice
                        [ ((string "quit" <|> string ":q") <* eof) $> pass
                        , (string "help" <* eof) $> do
                            outputStrLn helpText
                            loop chan
                        , (string "getEvent" <* eof) $> do
                            liftIO $ writeChan chan do
                                receivePayload >>= embed . printer
                            loop chan
                        , (reserved "channel" *> snowflakeP <* eof) <&> \id -> do
                            lift $ ST.put id
                            outputStrLn "switched to channel: "
                            liftIO $ writeChan chan do
                                getChannel id >>= embed . printer . responseBody
                            loop chan
                        , (reserved "delete" *> snowflakeP <* eof) <&> \msg_id -> do
                            chan_id <- lift ST.get
                            liftIO $ writeChan chan do
                                void $ deleteMessage chan_id msg_id
                            loop chan
                        , ((,) <$> lexeme (reserved "edit" *> snowflakeP) <*> (many1 anyChar <* eof))
                            <&> \(msg_id, new_conts) -> do
                                chan_id <- lift ST.get
                                liftIO $ writeChan chan do
                                    void $
                                        editMessage
                                            chan_id
                                            msg_id
                                            MessageEditForm
                                                { allowed_mentions = Nothing
                                                , embeds = Nothing
                                                , content = fromString new_conts
                                                , components = Nothing
                                                , flags = 1
                                                }
                                loop chan
                        , (reserved "send" *> many1 anyChar <* eof) <&> \message -> do
                            chan_id <- lift ST.get
                            liftIO $ writeChan chan do
                                void $
                                    createMessage
                                        chan_id
                                        MessageCreateForm
                                            { content = Just (fromString message)
                                            , tts = False
                                            , embeds = Nothing
                                            , nonce = Nothing
                                            , allowed_mentions = Nothing
                                            , message_reference = Nothing
                                            , components = Nothing
                                            , sticker_ids = Nothing
                                            , flags = 1
                                            }
                            loop chan
                        , (spaces <* eof) $> loop chan
                        ]

main :: IO ()
main = do
    token <- decodeUtf8 . BS.init <$> readFileBS "Token.dat"
    let identity =
            IdentifyStructure
                { token
                , properties =
                    ConnnectionProperties
                        { os = "linux"
                        , browser = ""
                        , device = ""
                        }
                , intents = 3665
                , shard = Just (0, 1)
                , compress = Nothing
                , large_treshold = Nothing
                , presence = Nothing
                }

    let botOpts = FumitoOpts {identity}
    runSecureClientWith
        "gateway.discord.gg"
        443
        "/"
        defaultConnectionOptions
        [("v", "10"), ("encoding", "json")]
        \connection -> new \di ->
            either throwIO pure
                =<< ( runM
                        . interpretReq
                        . runError
                        . asyncToIO
                        . runDiToIO di
                        . runWSToIO connection
                        . runReader botOpts
                        . runGateway
                        $ runChannelRequest bot
                    )
