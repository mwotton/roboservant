{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Breakdown where

import Data.Aeson
import Data.Hashable
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant

data Foo = Foo Int String
  deriving (Generic, Eq, Show, Typeable)

instance Hashable Foo

instance ToJSON Foo

instance FromJSON Foo

data SomeSum = A Int | B String
  deriving (Generic, Eq, Show, Typeable)

instance Hashable SomeSum

instance ToJSON SomeSum

instance FromJSON SomeSum

type ProductApi =
  "item" :> ReqBody '[JSON] Int :> Post '[JSON] ()
    :<|> "getFoo" :> Get '[JSON] Foo

eliminate :: Int -> Handler ()
eliminate _ = throwError $ err500 {errBody = "eliminate blew up, oh no!"}

productServer :: Server ProductApi
productServer = eliminate :<|> pure (Foo 12 "abc")

type SumApi =
  "item" :> ReqBody '[JSON] Int :> Post '[JSON] ()
    :<|> "getFoo1" :> Get '[JSON] SomeSum
    :<|> "getFoo2" :> Get '[JSON] SomeSum

sumServer :: Server SumApi
sumServer = eliminate :<|> pure (B "hi") :<|> pure (A 3)
