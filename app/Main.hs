module Main (main) where

import Prelude hiding (Reader, runReader)

import Fumito.Types

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

import Fumito.Gateway

import Control.Exception (throwIO)

import Data.Default (Default (def))
import Data.String.Interpolate
import Di qualified as D

gateway :: (Members [Async, Di D.Level D.Path D.Message, Embed IO, Error GatewayException, FumitoGateway] r) => Sem r ()
gateway = push "gateway" do
    notice @Text "Established connection with gateway"

    interval_ms <- receiveHelloEvent
    info @Text [i|Received heartbeat interval: #{interval_ms}ms|]

    void $ push "heartbeat" $ async do
        jitter <- randomRIO @Float (0, 1)
        embed $ threadDelay $ floor $ fromIntegral interval_ms * jitter * 1000
        sendHeartBeat
        infinitely do
            embed $ threadDelay $ fromInteger $ interval_ms * 1000
            sendHeartBeat

    sendIdentity >>= print

    void $ async do
        putStrLn "Type Input to close gateway"
        void $ embed getLine
        notice @Text "Closing Gateway Connection"
        closeGateway ("Disconnected" :: Text)

    void $ infinitely do
        sendPayload (HeartBeatSend (Just 2))
        try receiveDispatchEvent >>= \case
            Right (GUILD_CREATE n) -> print $ map permission_overwrites $ channels n
            _ -> pass
        embed $ threadDelay 1_000_000

main :: IO ()
main = do
    _token <- BS.init <$> readFileBS "Token.dat"
    let _identity =
            def
                { token = decodeUtf8 _token
                , properties = def {os = "linux"}
                , intents = 3665
                , shard = Just (0, 1)
                }
    _lastSequenceNum <- newIORef Nothing
    let botOpts = FumitoState {_lastSequenceNum, _identity, _fumito_resume_gateway_url = Nothing}
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
                        $ runFumitoGateway botOpts gateway
                    )
