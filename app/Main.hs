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

gateway :: (Members [Async, DiEffect, Embed IO, FumitoGateway] r) => Sem r ()
gateway = push "gateway" do
    notice @Text "Established connection with gateway"

    interval_ms <- (`div` 10) <$> receiveHelloEvent
    info @Text [i|Received heartbeat interval: #{interval_ms}ms|]

    void $ push "heartbeat" $ async do
        jitter <- randomRIO @Float (0, 1)
        embed $ threadDelay $ floor $ fromIntegral interval_ms * jitter * 1000
        -- TODO: get `_lastSequenceNum` from future state updates for `sendHeartBeat` calls
        sendHeartBeat
        infinitely do
            embed $ threadDelay $ fromInteger $ interval_ms * 1000
            sendHeartBeat
            getFumitoState >>= print

    _identity <- sendIdentity 


    void getLine
    notice @Text "Closing Gateway Connection"
    closeGateway ("Disconnected" :: Text)

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
    let botOpts = def {_identity}
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
