{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Prelude hiding (Reader, runReader)

import GHC.Conc

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
    receiveDispatchEvent,
    receiveHelloEvent,
    runGateway,
    sendIdentity,
    sendPayload,
 )
import Fumito.Gateway.Types (
    ConnectionProperties (..),
    IdentifyStructure (..),
    PayloadSend (..),
 )
import Fumito.HTTP.Client
import Fumito.Types.Exception
import Polysemy.Reader (runReader)
import Polysemy.Req (interpretReq)
import Shower

bot :: Members (ChannelRequest ': GatewayEff) r => Sem r ()
bot = push "gateway" do
    notice @Text "Established connection with gateway"
    receiveHelloEvent

    sendIdentity >>= embed . printer

    loop <- async $ infinitely do
        sendPayload (HeartBeatSend (Just 2))
        try receiveDispatchEvent >>= \case
            Right n -> embed $ printer n
            Left (EventMismatch _ _) -> pass
            e -> print e
        embed $ threadDelay 400_000

    putStrLn "Type Input to close gateway"
    void getLine
    notice @Text "Closing Gateway Connection"
    cancel loop
    closeGateway ("Disconnected" :: Text)

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
