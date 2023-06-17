module Polysemy.Websocket where

import Network.WebSockets qualified as NW
import Network.WebSockets.Connection qualified as NW
import Polysemy (Embed, Member, Members, Sem, embed, interpretH, makeSem, pureT, raiseUnder)
import Polysemy.Reader qualified as P

data WebSocket m a where
    Receive :: WebSocket m NW.Message
    ReceiveDataMessage :: WebSocket m NW.DataMessage
    Send :: NW.Message -> WebSocket m ()
    SendDataMessage :: NW.DataMessage -> WebSocket m ()
    SendDataMessages :: [NW.DataMessage] -> WebSocket m ()

makeSem ''WebSocket

runWSToIOReader :: Members '[Embed IO, P.Reader NW.Connection] r => Sem (WebSocket ': r) a -> Sem r a
runWSToIOReader = interpretH \case
    Send m -> P.ask >>= embed . flip NW.send m >>= pureT
    SendDataMessage m -> P.ask >>= embed . flip NW.sendDataMessage m >>= pureT
    SendDataMessages m -> P.ask >>= embed . flip NW.sendDataMessages m >>= pureT
    Receive -> do
        P.ask >>= embed . NW.receive >>= pureT
    ReceiveDataMessage ->
        P.ask >>= embed . NW.receiveDataMessage >>= pureT

runWSToIO :: Member (Embed IO) r => NW.Connection -> Sem (WebSocket ': r) a -> Sem r a
runWSToIO connection = P.runReader connection . runWSToIOReader . raiseUnder

receiveData :: (NW.WebSocketsData a, Member WebSocket r) => Sem r a
receiveData = NW.fromDataMessage <$> receiveDataMessage

sendTextDatas :: (NW.WebSocketsData a, Member WebSocket r) => [a] -> Sem r ()
sendTextDatas = sendDataMessages . map (\x -> NW.Text (NW.toLazyByteString x) Nothing)

sendTextData :: (NW.WebSocketsData a, Member WebSocket r) => a -> Sem r ()
sendTextData = sendTextDatas . one

sendClose :: (NW.WebSocketsData a, Member WebSocket r) => a -> Sem r ()
sendClose = sendCloseCode 1000

sendCloseCode :: (NW.WebSocketsData a, Member WebSocket r) => Word16 -> a -> Sem r ()
sendCloseCode code = send . NW.ControlMessage . NW.Close code . NW.toLazyByteString

sendPing :: (NW.WebSocketsData a, Member WebSocket r) => a -> Sem r ()
sendPing = send . NW.ControlMessage . NW.Ping . NW.toLazyByteString
