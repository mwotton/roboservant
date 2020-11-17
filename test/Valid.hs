{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Valid where

import Servant
import Data.Void

type Api = Get '[JSON] Int
      :<|> Capture "void" Void :> Get '[JSON] ()
       
server :: Server Api
server = pure 7 :<|> const (pure ())
