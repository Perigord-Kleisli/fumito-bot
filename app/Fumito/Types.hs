module Fumito.Types where

data FumitoOpts = FumitoOpts
    { token :: ByteString
    , dummy :: Int
    }
    deriving stock (Show)
