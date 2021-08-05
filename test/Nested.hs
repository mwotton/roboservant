{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Nested where

import Servant
import Servant.API.Flatten

type Api =
  ( "one" :> Summary "foo" :> Post '[JSON] Int
      :<|> "two" :> Post '[JSON] Int
  )
    :<|> ( "three" :> Post '[JSON] Int
         )

type FlatApi = Flat Api

server :: Server FlatApi
server = pure 1 :<|> pure 2 :<|> pure 3
