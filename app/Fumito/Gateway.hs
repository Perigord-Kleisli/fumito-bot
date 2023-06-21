{-# LANGUAGE OverloadedLabels #-}

module Fumito.Gateway where

import Control.Lens
import Data.Aeson (eitherDecode, eitherDecode', encode)
import Data.Aeson.Types (FromJSON)
import Data.Generics.Labels ()
import Data.String.Interpolate
import Di qualified as D
import DiPolysemy
import Fumito.Client.Types
import Fumito.Gateway.Types
import Fumito.Types.Exception
import Network.WebSockets qualified as NW
import Polysemy
import Polysemy.Error
import Polysemy.State as PS
import Polysemy.Websocket (WebSocket, receiveData, sendClose, sendTextData)

data FumitoGateway m a where
    SendPayload :: PayloadSend -> FumitoGateway m ()
    ReceivePayload :: FumitoGateway m PayloadReceive
    ReceivePayload' :: FumitoGateway m PayloadReceive
    GetFumitoState :: FumitoGateway m FumitoState
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
    GetFumitoState -> PS.get
    ReceivePayload -> receiveDecodeThrow >>= actOnInput
    ReceivePayload' -> receiveDecodeThrow' >>= actOnInput
    CloseGateway message -> do
        sendClose message
    where
        actOnInput :: PayloadReceive -> Sem r PayloadReceive
        actOnInput dispatch@(Dispatch {s = s, d = READY (ReadyStructure {resume_gateway_url})}) = do
            PS.gets lastSequenceNum >>= (`writeIORef` Just s)
            PS.modify (#fumito_resume_gateway_url ?~ resume_gateway_url)
            return dispatch
        actOnInput dispatch@(Dispatch {s}) = do
            PS.gets lastSequenceNum >>= (`writeIORef` Just s)
            return dispatch
        actOnInput x = return x

--
runFumitoGateway :: Members GatewayEffects r => FumitoState -> Sem (FumitoGateway ': r) a -> Sem r a
runFumitoGateway fumitoOpts = PS.evalLazyState fumitoOpts . runFumitoGatewayToReader . raiseUnder

--
sendIdentity :: Member FumitoGateway r => Sem r PayloadReceive
sendIdentity = do
    ident <- (^. #identity) <$> getFumitoState
    sendPayload (Identify ident)
    receivePayload

receiveDispatchEvent :: Members '[Error GatewayException, FumitoGateway] r => Sem r DispatchEvent
receiveDispatchEvent = do
    receivePayload >>= \case
        Dispatch {d} -> return d
        e -> throw (EventMismatch "DispatchEvent" e)

receiveHelloEvent :: Members '[Error GatewayException, FumitoGateway] r => Sem r Integer
receiveHelloEvent = do
    receivePayload' >>= \case
        HelloEvent interval -> return interval
        e -> throw (EventMismatch "HelloEvent" e)

sendHeartBeat :: Members '[Embed IO, Di D.Level D.Path D.Message, FumitoGateway] r => Sem r ()
sendHeartBeat = do
    last_s <- embed . readIORef . lastSequenceNum =<< getFumitoState
    sendPayload (HeartBeatSend last_s)
    let last_s' = maybe "null" (show @Text) last_s
    info @Text [i|Sent heartbeat event with sequence number `#{last_s'}`|]
