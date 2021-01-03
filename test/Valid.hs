{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Valid where

import Data.Void
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic

type Api =
  Get '[JSON] Int
    :<|> Capture "void" Void :> Get '[JSON] ()

data Routes route
  = Routes
      { getInt ::
          route
            :- Get '[JSON] Int,
        captureIt ::
          route
            :- Capture "void" Void :> Get '[JSON] ()
      }
  deriving (Generic)

type RoutedApi = ToServantApi Routes

-- routedApi = genericApi (Proxy :: Proxy Routes)
routedServer :: Server RoutedApi
routedServer = genericServer routes

routes :: Routes AsServer
routes =
  Routes
    { getInt = pure 7,
      captureIt = const (pure ())
    }

server :: Server Api
server = pure 7 :<|> const (pure ())
