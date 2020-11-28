{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Foo where

import Data.Aeson
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant
import Data.Hashable

newtype Foo = Foo Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance Hashable Foo
instance ToJSON Foo
instance FromJSON Foo

type Api =
  "item" :> Get '[JSON] Foo
    :<|> "itemAdd" :> Capture "one" Foo :> Capture "two" Foo :> Get '[JSON] Foo
    :<|> "item" :> Capture "itemId" Foo :> Get '[JSON] ()

intro :: Handler Foo
intro = pure (Foo 1)

combine :: Foo -> Foo -> Handler Foo
combine (Foo a) (Foo b) = pure (Foo (a + b))

eliminate :: Foo -> Handler ()
eliminate (Foo a)
  | a > 10 = throwError $ err500 {errBody = "eliminate blew up, oh no!"}
  | otherwise = pure ()

server :: Server Api
server =
  intro
    :<|> combine
    :<|> eliminate
