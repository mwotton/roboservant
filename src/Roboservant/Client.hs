{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- should all the NormalizeFunction instances be in one place?
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Roboservant.Client where

import Data.Proxy
import Servant.Client
import Roboservant.Types
import Roboservant(Report, fuzz')
import Servant
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Dynamic (Dynamic,Typeable)
import qualified Data.Vinyl.Curry as V
import qualified Data.Text as T
import Control.Monad.Reader
import Data.Hashable
import Network.HTTP.Types.Status

-- fuzz :: forall api.
--               (FlattenServer api, ToReifiedApi (Endpoints api)) =>
--               Server api ->
--               Config ->
--               IO (Maybe Report)
-- fuzz s  = fuzz' (reifyServer s)
--   -- todo: how do we pull reifyServer out?
--   where reifyServer :: (FlattenServer api, ToReifiedApi (Endpoints api))
--                     => Server api -> ReifiedApi
--         reifyServer server = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))

fuzz :: forall api . (ToReifiedClientApi (Endpoints api), FlattenClient api, HasClient ClientM api)
     => ClientEnv -> Config -> IO (Maybe Report)
fuzz clientEnv
  = fuzz'
      (toReifiedClientApi
         (flattenClient @api apiClient) (Proxy @(Endpoints api)) clientEnv)
  where apiClient = client (Proxy @api)



class ToReifiedClientApi api where
  toReifiedClientApi :: ClientBundled api -> Proxy api -> ClientEnv -> ReifiedApi

data ClientBundled endpoints where
  AClientEndpoint :: Client ClientM endpoint -> ClientBundled endpoints -> ClientBundled (endpoint ': endpoints)
  NoClientEndpoints :: ClientBundled '[]


class FlattenClient api where
  flattenClient :: Client ClientM api  -> ClientBundled (Endpoints api)

instance
  ( NormalizeFunction (Client ClientM endpoint)
  , Normal (Client ClientM endpoint) ~ V.Curried (EndpointArgs endpoint) (ReaderT ClientEnv IO (Either InteractionError (NonEmpty (Dynamic,Int))))
  , ToReifiedClientApi endpoints
  , V.RecordCurry' (EndpointArgs endpoint)
  , ToReifiedEndpoint endpoint) =>
  ToReifiedClientApi (endpoint : endpoints) where
  toReifiedClientApi (endpoint `AClientEndpoint` endpoints) _ clientEnv =
    (0, ReifiedEndpoint
        { reArguments    = reifiedEndpointArguments @endpoint
        , reEndpointFunc = foo (normalize endpoint)
        }
    )
    : (map . first) (+1)
    (toReifiedClientApi endpoints (Proxy @endpoints) clientEnv)
    where

      foo :: V.Curried (EndpointArgs endpoint) (ReaderT ClientEnv IO ResultType)
          -> V.Curried (EndpointArgs endpoint) (IO ResultType)
      foo = mapCurried @(EndpointArgs endpoint) @(ReaderT ClientEnv IO ResultType) (`runReaderT` clientEnv)

mapCurried :: forall ts a b. V.RecordCurry' ts => (a -> b) -> V.Curried ts a -> V.Curried ts b
mapCurried f g = V.rcurry' @ts $ f . V.runcurry' g

type ResultType = Either InteractionError (NonEmpty (Dynamic,Int))
-- runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)


instance (Typeable x, Hashable x, Breakdown x) => NormalizeFunction (ClientM x) where
  type Normal (ClientM x) = ReaderT ClientEnv IO (Either InteractionError (NonEmpty (Dynamic,Int)))
  normalize c = ReaderT $
    fmap (bimap renderClientError breakdown) . runClientM c
    where
      renderClientError :: ClientError -> InteractionError
      renderClientError err = case err of
        FailureResponse _ (Response{responseStatusCode}) -> InteractionError textual (responseStatusCode == status500)
        _ -> InteractionError textual True

        where textual = T.pack $ show err
instance ToReifiedClientApi '[] where
  toReifiedClientApi NoClientEndpoints _ _ = []


instance
  ( FlattenClient api,
    Endpoints endpoint ~ '[endpoint]
  ) =>
  FlattenClient (endpoint :<|> api)
  where
  flattenClient (endpoint :<|> c) = endpoint `AClientEndpoint` flattenClient @api c

instance
 (
   Endpoints api ~ '[api]
 ) =>
  FlattenClient (x :> api)
  where
  flattenClient c = c `AClientEndpoint` NoClientEndpoints


instance FlattenClient (Verb method statusCode contentTypes responseType)
  where
  flattenClient c = c `AClientEndpoint` NoClientEndpoints
