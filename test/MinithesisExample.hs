{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MinithesisExample where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Servant

newtype Secret = Secret Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

type Api =
       "noise" :> "alpha" :> Get '[JSON] Int
  :<|> "noise" :> "beta" :> Get '[JSON] Int
  :<|> "noise" :> "gamma" :> Get '[JSON] Int
  :<|> "noise" :> "delta" :> Get '[JSON] ()
  :<|> "noise" :> "epsilon" :> Get '[JSON] Int
  :<|> "seed" :> Get '[JSON] Secret
  :<|> "noise" :> "zeta" :> Get '[JSON] ()
  :<|> "noise" :> "eta" :> Get '[JSON] Int
  :<|> "fail" :> Capture "secret" Secret :> Get '[JSON] ()

server :: Server Api
server =
       pure 1
  :<|> pure 2
  :<|> pure 3
  :<|> pure ()
  :<|> pure 4
  :<|> pure targetSecret
  :<|> pure ()
  :<|> pure 5
  :<|> failWithSecret

targetSecret :: Secret
targetSecret = Secret 777

failWithSecret :: Secret -> Handler ()
failWithSecret secret
  | secret == targetSecret = throwError err500 {errBody = "explosion"}
  | otherwise = pure ()
