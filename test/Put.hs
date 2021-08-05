{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}


{-# LANGUAGE TypeOperators #-}

module Put where

import Data.Aeson
import Data.Hashable
import GHC.Generics (Generic)
import Servant

type Api =
  Get '[JSON] Foo
    :<|> ReqBody '[JSON] Foo :> Put '[JSON] ()
    :<|> ReqBody '[JSON] Foo :> Put '[JSON] NoContent

data Foo = Foo
  deriving (Eq, Show, Generic)

instance Hashable Foo

instance ToJSON Foo

instance FromJSON Foo

server :: Server Api
server =
  pure Foo
    :<|> const (pure ())
    :<|> const (pure NoContent)
