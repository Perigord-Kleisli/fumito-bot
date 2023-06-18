module Fumito.Types.Exception where

import GHC.Show (Show (show))
import Network.WebSockets

import Data.Aeson (Value, decode')
import Data.String.Interpolate

data GatewayException
    = JsonParseErr {err :: String, input :: LByteString}
    | WebSocket ConnectionException
    deriving anyclass (Exception)

instance Show GatewayException where
    show (JsonParseErr {err, input}) =
        let result = GHC.Show.show $ decode' @Value input
         in [__i|
        JsonParserError
            error: #{err}
            originalInput: ```
            #{input}
            ```
            Value parsing: ```
            #{result}
            ```
        |]
    show (WebSocket x) = GHC.Show.show x
