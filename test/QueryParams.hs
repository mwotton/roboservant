{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DerivingStrategies #-}


{-# LANGUAGE TypeOperators #-}

module QueryParams where

import Servant

type Api = QueryParams "ints" Int :> Get '[JSON] [Int]

server :: Server Api
server  = pure
