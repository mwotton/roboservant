{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | terrible name, this really just pulls stuff out where we can fiddle with it.

module Roboservant.Types.FlattenServer where

import Servant

data Bundled endpoints where
  AnEndpoint :: Server endpoint -> Bundled endpoints -> Bundled (endpoint ': endpoints)
  NoEndpoints :: Bundled '[]

class FlattenServer api where
  flattenServer :: Server api -> Bundled (Endpoints api)


instance
  ( FlattenServer api
  , Endpoints endpoint ~ '[endpoint]
  ) =>
  FlattenServer (endpoint :<|> api)
  where
  flattenServer (endpoint :<|> server) = endpoint `AnEndpoint` flattenServer @api server

instance
  ( HasServer (x :> api) '[],
    Endpoints (x :> api) ~ '[x :> api]
  ) =>
  FlattenServer (x :> api)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

instance
  (  Endpoints (Verb method statusCode contentTypes responseType) ~ '[Verb method statusCode contentTypes responseType],
     HasServer (Verb method statusCode contentTypes responseType) '[]

  ) =>
  FlattenServer (Verb method statusCode contentTypes responseType)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints
