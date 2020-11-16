{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Valid where

import Data.Aeson
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant

type Api = Get '[JSON] Int

server :: Server Api
server = pure 7
