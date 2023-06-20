-- "Network.HTTP.Req" adapted for use with polysemy.
-- Copied verbatim from <https://hackage.haskell.org/package/polysemy-req-0.1.0>
module Polysemy.Req (
    -- * Effect
    Req (..),

    -- * Actions
    req,

    -- * Interpretations
    interpretReq,
    interpretReqWith,

    -- * Re-exports
    module Network.HTTP.Req,
)
where

import Network.HTTP.Req hiding (MonadHttp, Req, req, req', reqBr, reqCb, runReq)
import Network.HTTP.Req qualified as R
import Polysemy

{- | An effect for making http 'Network.HTTP.Req.req'uests.
 @since 0.1.0
-}
data Req m response where
    Req ::
        ( HttpMethod method
        , HttpBody body
        , HttpResponse response
        , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
        ) =>
        -- | HTTP method
        method ->
        -- | 'Url'—location of resource
        Url scheme ->
        -- | Body of the request
        body ->
        -- | A hint how to interpret response
        Proxy response ->
        -- | Collection of optional parameters
        Option scheme ->
        -- | Response
        Req m response

{- | See 'Network.HTTP.Req.req'.
 @since 0.1.0
-}
makeSem ''Req

{- | Run a 'Req' effect with the 'Network.HTTP.Req.defaultHttpConfig'.
 @since 0.1.0
-}
interpretReq :: Member (Embed IO) r => InterpreterFor Req r
interpretReq = interpretReqWith defaultHttpConfig

{- | Run a 'Req' effect with a custom 'Network.HTTP.Req.HttpConfig'.
 @since 0.1.0
-}
interpretReqWith :: Member (Embed IO) r => HttpConfig -> InterpreterFor Req r
interpretReqWith cfg = interpret $ \case
    Req m u b p o -> embed @IO $ R.runReq cfg $ R.req @R.Req m u b p o
