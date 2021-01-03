{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Product where

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

type Api = "item" :> ReqBody '[JSON] Foo :> Post '[JSON] ()

eliminate :: Foo -> Handler ()
eliminate (Foo _a _b) = throwError $ err500 {errBody = "eliminate blew up, oh no!"}

server :: Server Api
server = eliminate
