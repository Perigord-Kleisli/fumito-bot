module Main (main) where

import Prelude hiding (Reader, runReader)

import Fumito.Types
import Fumito.Types.Gateway

import GHC.Conc
import System.Random

import Network.WebSockets (defaultConnectionOptions)
import Wuss

import Di (Level, Message, Path, new)

import Data.Aeson

import Data.ByteString qualified as BS
import Data.Default
import DiPolysemy
import Polysemy
import Polysemy.Async
import Polysemy.Reader qualified as R
import Polysemy.Websocket

import Shower

gateway :: Members '[Di Level Path Message, Async, WebSocket, R.Reader FumitoOpts, Embed IO] r => Sem r ()
gateway = push "gateway" do
    notice @Text "Starting Gateway Connection"
    notice @Text "Press Enter to Exit"
    interval_ms <-
        receiveData
            >>= fmap (heartbeat_interval . d)
                . embed
                . either fail pure
                . eitherDecode

    void $ push "heartbeat" $ async $ infinitely do
        jitter <- randomRIO @Float (0, 1)
        embed $ threadDelay $ floor $ fromIntegral interval_ms * jitter * 1000
        sendTextData $
            encode
                Payload
                    { op = Heartbeat
                    , d = Nothing @Int
                    , s = Nothing
                    , t = Nothing
                    }
        info @Text "sent heartbeat event"

    botToken <- R.asks (\x -> x.token)

    let botIdentity =
            def
                { token = decodeUtf8 botToken
                , intents = 3665
                , properties = def {os = "linux"}
                , shard = Just (0, 1)
                }

    embed $ printer botIdentity
    sendTextData $
        encode $
            Payload
                { op = Identify
                , d = botIdentity
                , s = Nothing
                , t = Nothing
                }
    readyEvent <- receiveData >>= embed . either fail pure . eitherDecode @(GatewayEventPayload ReadyStructure)
    embed $ printer readyEvent

    void $ embed getLine
    notice @Text "Closing Gateway Connection"
    sendClose @Text "Disconnected"

main :: IO ()
main = do
    token <- BS.init <$> readFileBS "Token.dat"
    let botOpts = FumitoOpts {token, dummy = 1}
    runSecureClientWith
        "gateway.discord.gg"
        443
        "/"
        defaultConnectionOptions
        [("v", "10"), ("encoding", "json")]
        \connection -> new \di ->
            runM
                . asyncToIO
                . runDiToIO di
                . R.runReader botOpts
                $ runWSToIO connection gateway
