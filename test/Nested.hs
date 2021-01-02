{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Nested where

import Servant
import Servant.API.Flatten


type Api =
  ("one" :> Post '[JSON] Int
   :<|> "two" :> Post '[JSON] Int
  )
  :<|> (
  "three" :> Post '[JSON] Int
  )

type FlatApi = Flat Api

server :: Server FlatApi
server = pure 1 :<|> pure 2 :<|> pure 3
