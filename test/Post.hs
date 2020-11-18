{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Post where

import GHC.Generics (Generic)
import Servant
import Data.Aeson

type Api = Get '[JSON] FooPost
      :<|> ReqBody '[JSON] FooPost :> Post '[JSON] ()

data FooPost = FooPost
  deriving (Eq,Show,Generic)

instance ToJSON FooPost
instance FromJSON FooPost

server :: Server Api
server = pure FooPost
  :<|> const (pure ())
