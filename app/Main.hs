{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Prelude hiding (Reader, runReader)

import GHC.Conc
import System.Random

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

import Data.String.Interpolate
import Fumito.Client.Types
import Fumito.Gateway (
    GatewayEffC,
    closeGateway,
    receiveDispatchEvent,
    receiveHelloEvent,
    runGateway,
    sendHeartBeat,
    sendIdentity,
    sendPayload, heartBeatLoop,
 )
import Fumito.Gateway.Types (
    ConnectionProperties (..),
    IdentifyStructure (..),
    PayloadSend (..),
    UpdatePrescense (..),
 )
import Fumito.Types.Exception
import Polysemy.Reader (runReader)
import Shower

gateway :: GatewayEffC r => Sem r ()
gateway = push "gateway" do
    notice @Text "Established connection with gateway"

    interval_ms <- fmap (*1000) receiveHelloEvent
    info @Text [i|Received heartbeat interval: #{interval_ms}ms|]

    void $ push "heartbeat" $ async do
        jitter <- randomRIO @Float (0, 1)
        embed $ threadDelay $ floor $ fromIntegral interval_ms * jitter
        sendHeartBeat
        heartBeatLoop interval_ms

    sendIdentity >>= embed . printer

    void $ async do
        putStrLn "Type Input to close gateway"
        void $ embed getLine
        notice @Text "Closing Gateway Connection"
        closeGateway ("Disconnected" :: Text)

    void $ infinitely do
        sendPayload (HeartBeatSend (Just 2))
        try receiveDispatchEvent >>= \case
            Right n -> embed $ printer n
            Left (EventMismatch _ _) -> pass
            e -> print e
        embed $ threadDelay 1_000_000

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
                , presence =
                    Just $
                        UpdatePrescense
                            { since = Nothing
                            , afk = False
                            , activities = []
                            , status = "being cool"
                            }
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
                        . runError
                        . asyncToIO
                        . runDiToIO di
                        . runWSToIO connection
                        . runReader botOpts
                        $ runGateway gateway
                    )
