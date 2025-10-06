{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MinithesisAuth where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Servant

newtype Token = Token Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

type Api =
  "login" :> Get '[JSON] Token
    :<|> "protected" :> Capture "token" Token :> Get '[JSON] ()

server :: Server Api
server =
  pure (Token 42)
    :<|> protected

protected :: Token -> Handler ()
protected token
  | token == Token 42 = pure ()
  | otherwise = throwError err401 {errBody = "unauthorized"}
