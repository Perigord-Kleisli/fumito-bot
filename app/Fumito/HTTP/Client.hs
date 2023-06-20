module Fumito.HTTP.Client where

import Polysemy
import Polysemy.Req

import Data.String.Interpolation

import Data.ByteString qualified as B

reqMain :: Members [Embed IO, Req] r => Sem r ()
reqMain = do
    token <- B.init <$> readFileBS "Token.dat"
    req
        GET
        (https "discord.com" /: "api" /: "v10" /: "channels" /: "951452676665794603")
        NoReqBody
        bsResponse
        (header "Authorization" [i|Bot ${token}])
        >>= print . responseBody

runReqMain = do
    runM $ interpretReqWith defaultHttpConfig reqMain
