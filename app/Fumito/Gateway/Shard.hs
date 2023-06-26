module Fumito.Gateway.Shard (
    Gateway,
    GatewayEff,
    GatewayEffC,
    sendPayload,
    receivePayload,
    receivePayload',
    gatewayStateGet,
    closeGateway,
    receiveDecodeThrow,
    receiveDecodeThrow',
    sendIdentity,
    receiveDispatchEvent,
    receiveHelloEvent,
    sendHeartBeat,
    gatewayStateGets,
    runGatewayToState,
    runGateway,
    heartBeatLoop,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Lens ((.~), (?~), (^.))
import Data.Aeson (eitherDecode, eitherDecode', encode)
import Data.Aeson.Types (FromJSON)
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import DiPolysemy (debug, push)
import Fumito.Client.Types (FumitoOpts (..))
import Fumito.Gateway.Types
import Fumito.Types.Common (LogEff)
import Fumito.Types.Exception
import Network.WebSockets qualified as NW
import Polysemy
import Polysemy.Async qualified as PA
import Polysemy.AtomicState as PS
import Polysemy.Error (Error, throw)
import Polysemy.Reader qualified as PR
import Polysemy.Websocket (WebSocket, receiveData, sendClose, sendCloseCode, sendTextData)
import System.Random

data ShardState = ShardState
    { heartBeatAckResponse :: Bool
    , heartBeatThread :: Maybe (Async (Maybe ()))
    , wsResumeURL :: Maybe Text
    , seqNum :: Maybe Int
    }
    deriving stock (Generic)

data Gateway m a where
    SendPayload :: PayloadSend -> Gateway m ()
    ReceivePayload :: Gateway m PayloadReceive
    ReceivePayload' :: Gateway m PayloadReceive
    -- | Atomically gets the state of the gateway client
    GatewayStateGet :: Gateway m ShardState
    GatewayStateModify :: (ShardState -> ShardState) -> Gateway m ()
makeSem ''Gateway

type GatewayEff =
    '[ Gateway
     , LogEff
     , Embed IO
     , PA.Async
     , Error GatewayException
     , PR.Reader FumitoOpts
     , WebSocket
     ]

type GatewayEffC r = Members GatewayEff r

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

type family Tail (xs :: [a]) where
    Tail (x ': xs) = xs

runGatewayToState ::
    forall a r.
    ( Members (PS.AtomicState ShardState ': Tail GatewayEff) r
    ) =>
    Sem (Gateway ': r) a ->
    Sem r a
runGatewayToState = interpret \case
    SendPayload payload -> actOnSend payload >>= sendTextData . encode
    GatewayStateGet -> PS.atomicGet
    GatewayStateModify f -> PS.atomicModify' f
    ReceivePayload -> receiveDecodeThrow >>= actOnInput
    ReceivePayload' -> receiveDecodeThrow' >>= actOnInput
    where
        actOnSend :: PayloadSend -> Sem r PayloadSend
        actOnSend payload = do
            return payload
        actOnInput :: PayloadReceive -> Sem r PayloadReceive
        actOnInput dispatch = do
            case dispatch of
                (Dispatch {s = s, d = READY (ReadyStructure {resume_gateway_url})}) -> do
                    PS.atomicModify $
                        (#wsResumeURL ?~ resume_gateway_url)
                            . (#seqNum ?~ s)
                (Dispatch {s}) -> do PS.atomicModify (#seqNum ?~ s)
                HeartBeatAck -> do
                    debug @Text "Received heartbeat ack"
                    PS.atomicModify' (#heartBeatAckResponse .~ True)
                _ -> pass
            return dispatch

runGateway :: Members (Tail GatewayEff) r => Sem (Gateway ': r) a -> Sem r a
runGateway = fmap snd . PS.atomicStateToIO shardState . runGatewayToState . raiseUnder
    where
        shardState =
            ShardState
                { heartBeatAckResponse = False
                , wsResumeURL = Nothing
                , seqNum = Nothing
                , heartBeatThread = Nothing
                }

closeGateway :: (NW.WebSocketsData a, GatewayEffC r) => a -> Sem r ()
closeGateway message = do
    haltHeartBeat
    sendClose message

sendIdentity :: Members GatewayEff r => Sem r PayloadReceive
sendIdentity = do
    ident <- PR.asks (^. #identity)
    sendPayload (Identify ident)
    receivePayload

receiveDispatchEvent :: GatewayEffC r => Sem r DispatchEvent
receiveDispatchEvent = do
    receivePayload >>= \case
        Dispatch {d} -> return d
        e -> throw (EventMismatch "DispatchEvent" e)

receiveHelloEvent :: GatewayEffC r => Sem r ()
receiveHelloEvent = push "Heartbeat" do
    receivePayload' >>= \case
        HelloEvent interval_ms -> do
            jitter <- embed $ randomRIO @Float (0, 1)
            embed $ threadDelay $ floor $ fromIntegral interval_ms * jitter
            haltHeartBeat
            thread <- PA.async $ heartBeatLoop interval_ms
            gatewayStateModify (#heartBeatThread ?~ thread)
        e -> throw (EventMismatch "HelloEvent" e)

gatewayStateGets :: Member Gateway r => (ShardState -> a) -> Sem r a
gatewayStateGets = (<$> gatewayStateGet)

sendHeartBeat :: GatewayEffC r => Sem r ()
sendHeartBeat = do
    seqNum' <- gatewayStateGets (^. #seqNum)
    debug @Text [i|Sending heartbeat with sequence number `#{seqNum'}`|]
    sendPayload (HeartBeatSend seqNum')
    gatewayStateModify (#heartBeatAckResponse .~ False)

haltHeartBeat :: GatewayEffC r => Sem r ()
haltHeartBeat = do
    thread <- gatewayStateGets (^. #heartBeatThread)
    gatewayStateModify (#heartBeatThread .~ Nothing)
    case thread of
        Just t -> do
            debug @Text "Stopping heartbeat thread"
            PA.cancel t
        Nothing -> pass

-- | Takes an interval in milliseconds
heartBeatLoop :: GatewayEffC r => Int -> Sem r ()
heartBeatLoop interval_ms = void $ infinitely do
    sendHeartBeat
    embed . threadDelay $ interval_ms * 1000
    unlessM (gatewayStateGets (^. #heartBeatAckResponse)) do
        debug @Text "No heartbeat response"
        sendCloseCode 4000 ("No heartbeat in time" :: Text)
