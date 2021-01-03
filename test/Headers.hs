{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Headers where

import Data.Aeson
import Data.Hashable
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant

newtype Foo = Foo Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance Hashable Foo

instance ToJSON Foo

instance FromJSON Foo

type Api =
  "item" :> Get '[JSON] Foo
    :<|> "itemAdd" :> Header "one" Foo :> Header "two" Foo :> Get '[JSON] Foo
    :<|> "item" :> Capture "itemId" Foo :> Get '[JSON] ()

intro :: Handler Foo
intro = pure (Foo 1)

combine :: Maybe Foo -> Maybe Foo -> Handler Foo
combine (Just (Foo a)) (Just (Foo b)) = pure (Foo (a + b))
combine (Just a) Nothing = pure a
combine Nothing (Just a) = pure a
combine Nothing Nothing = pure (Foo 1)

eliminate :: Foo -> Handler ()
eliminate (Foo a)
  | a > 10 = throwError $ err500 {errBody = "eliminate blew up, oh no!"}
  | otherwise = pure ()

server :: Server Api
server =
  intro
    :<|> combine
    :<|> eliminate
