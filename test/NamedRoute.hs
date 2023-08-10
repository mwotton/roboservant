
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}


{-# LANGUAGE TypeOperators #-}

module NamedRoute where

import Data.Aeson
import Data.Hashable
import GHC.Generics (Generic)
import Servant
import Servant.API.NamedRoutes

type API = NamedRoutes NamedAPI

data NamedAPI mode =
  NamedAPI
  { putFoo :: mode :- "foo" :> ReqBody '[JSON] Foo :> Put '[JSON] ()
  , getFoo :: mode :- "foo" :> Get '[JSON] Foo
  } deriving Generic

data Foo = Foo
  deriving (Eq, Show, Generic)

instance Hashable Foo

instance ToJSON Foo

instance FromJSON Foo

server :: Server API
server = NamedAPI
  { putFoo = const (pure ())
  , getFoo = pure Foo
  }
