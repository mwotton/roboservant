{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Seeded where

import Data.Aeson
import Data.Hashable
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.Except (throwError)
import Servant.Server (err500)

newtype Seed = Seed Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance ToJSON Seed

instance FromJSON Seed

instance Hashable Seed

type Api =
  Capture "seed" Seed :> Get '[JSON] ()
    :<|> Get '[JSON] ()

server :: Server Api
server =
  (\(Seed _) -> throwError err500 { errBody = LBS.pack "boom" })
    :<|> pure ()
