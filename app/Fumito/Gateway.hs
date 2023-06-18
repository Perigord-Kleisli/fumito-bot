module Fumito.Gateway where

import Prelude hiding (Reader, ask, asks)

import Control.Lens ((?~))
import Data.Aeson (eitherDecode, eitherDecode', encode)
import Data.Aeson.Types (FromJSON)
import Data.String.Interpolate
import Di qualified as D
import DiPolysemy
import Fumito.Types
import Network.WebSockets qualified as NW
import Polysemy
import Polysemy.Error
import Polysemy.State as PS
import Polysemy.Websocket (WebSocket, receiveData, sendClose, sendTextData)

data FumitoGateway m a where
    SendPayload :: PayloadSend -> FumitoGateway m ()
    SendIdentity :: FumitoGateway m (DispatchEvent ReadyStructure)
    ReceiveDirectData :: FumitoGateway m LByteString
    GetFumitoState :: FumitoGateway m FumitoState
    ReceiveHelloEvent :: FumitoGateway m Integer
    ReceiveHeartBeatAck :: FumitoGateway m (Maybe Int)
    ReceiveDispatchEvent :: FromJSON a => FumitoGateway m (DispatchEvent a)
    CloseGateway :: NW.WebSocketsData a => a -> FumitoGateway m ()
makeSem ''FumitoGateway

type GatewayEffects = '[Di D.Level D.Path D.Message, Embed IO, Error GatewayException, WebSocket]

receiveDecodeThrow :: (FromJSON a, Members '[Error GatewayException, WebSocket] r) => Sem r a
receiveDecodeThrow = do
    input <- receiveData
    case eitherDecode input of
        Left e -> do
            throw (JsonParseErr e input)
        Right x -> return x

--
receiveDecodeThrow' :: (FromJSON a, Members '[Error GatewayException, WebSocket] r) => Sem r a
receiveDecodeThrow' = do
    input <- receiveData
    case eitherDecode' input of
        Left e -> do
            throw (JsonParseErr e input)
        Right x -> return x

runFumitoGatewayToReader :: forall a r. (Members (PS.State FumitoState ': GatewayEffects) r) => Sem (FumitoGateway ': r) a -> Sem r a
runFumitoGatewayToReader = interpret \case
    SendPayload payload -> sendTextData (encode payload)
    ReceiveDirectData -> receiveData
    GetFumitoState -> PS.get
    ReceiveHelloEvent -> heartbeat_interval <$> receiveDecodeThrow'
    ReceiveHeartBeatAck -> zombified_value <$> receiveDecodeThrow'
    ReceiveDispatchEvent -> do
        dispatchEvent <- receiveDecodeThrow'
        PS.modify (lastSequenceNum ?~ s dispatchEvent)
        return (d dispatchEvent)
    SendIdentity -> do
        iden <- _identity <$> PS.get
        sendTextData (encode (Identify iden))
        readyStructure <- receiveDecodeThrow'
        PS.modify
            ( (lastSequenceNum ?~ s readyStructure)
                . (fumito_resume_gateway_url ?~ readyStructure.d.fromDispatch.resume_gateway_url)
            )
        return (d readyStructure)
    CloseGateway message -> do
        sendClose message

runFumitoGateway :: Members GatewayEffects r => FumitoState -> Sem (FumitoGateway ': r) a -> Sem r a
runFumitoGateway fumitoOpts = PS.evalLazyState fumitoOpts . runFumitoGatewayToReader . raiseUnder

sendHeartBeat :: Members '[DiEffect, FumitoGateway] r => Sem r ()
sendHeartBeat = do
    last_s <- _lastSequenceNum <$> getFumitoState
    sendPayload (HeartBeatSend last_s)
    let last_s' = maybe "null" (show @Text) last_s
    info @Text [i|Sent heartbeat event with sequence number `#{last_s'}`|]
