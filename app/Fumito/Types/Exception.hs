module Fumito.Types.Exception where

import Data.String.Interpolate (i, __i)
import Fumito.Types.Gateway (PayloadReceive)
import GHC.Show (Show (show))
import Network.WebSockets (ConnectionException)

data GatewayException
    = JsonParseErr {err :: String, input :: LByteString}
    | EventMismatch {expectedEvent :: Text, receivedEvent :: PayloadReceive}
    | InvalidPayloadReceive {reason :: Text}
    | WebSocket ConnectionException
    deriving anyclass (Exception)

instance Show GatewayException where
    show (JsonParseErr {err, input}) =
        [__i|
        JsonParserError
            error: #{err}
            originalInput: ```
            #{input}
            ```
        |]
    show (EventMismatch {expectedEvent, receivedEvent}) =
        [__i|
        Event Mismatch:
            Expected event #{expectedEvent} but got #{receivedEvent}
        |]
    show (InvalidPayloadReceive reason) =
        [i|Received invalid input from server, Reason: #{reason}|]
    show (WebSocket x) = GHC.Show.show x
