{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Roboservant.Types.ReifiedApi.Server(module Roboservant.Types.ReifiedApi.Server) where

import Servant

import Control.Monad.Except (runExceptT)
import Data.Bifunctor
import Data.Dynamic (Dynamic)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import Roboservant.Types.Breakdown
import Roboservant.Types.ReifiedApi

import qualified Data.Text as T
import qualified Data.Vinyl.Curry as V
import Data.Hashable(Hashable)

type ReifiedApi = [(ApiOffset, ReifiedEndpoint )]


class ToReifiedApi endpoints where
  toReifiedApi :: Bundled endpoints -> Proxy endpoints -> ReifiedApi

instance ToReifiedApi '[] where
  toReifiedApi NoEndpoints _ = []

instance
  ( NormalizeFunction (ServerT endpoint Handler)
  , Normal (ServerT endpoint Handler) ~ V.Curried (EndpointArgs endpoint) (IO (Either InteractionError (NonEmpty (Dynamic,Int))))
  , ToReifiedEndpoint endpoint
  , ToReifiedApi endpoints
  ) =>
  ToReifiedApi (endpoint : endpoints)
  where
  toReifiedApi (endpoint `AnEndpoint` endpoints) _ =
    (0, ReifiedEndpoint
         { reArguments    = reifiedEndpointArguments @endpoint
         , reEndpointFunc = normalize endpoint
         }
    )
      : (map . first) (+1)
        (toReifiedApi endpoints (Proxy @endpoints))


instance (Typeable x, Hashable x, Breakdown x) => NormalizeFunction (Handler x) where
  type Normal (Handler x) = IO (Either InteractionError (NonEmpty (Dynamic,Int)))
  normalize handler = (runExceptT . runHandler') handler >>= \case
    Left serverError -> pure (Left (renderServerError serverError))
      where
        -- | TODO improve this
        renderServerError :: ServerError -> InteractionError
        renderServerError = InteractionError . T.pack . show
    Right x -> pure $ Right $ breakdown x


--          case errHTTPCode serverError of
--            500 -> throw serverError
--            _ ->
--              liftIO . logInfo . show $ ("ignoring non-500 error", serverError)


data Bundled endpoints where
  -- AnEndpoint :: Server endpoint -> Bundled endpoints -> Bundled (endpoint ': endpoints)
  AnEndpoint :: Server endpoint -> Bundled endpoints -> Bundled (endpoint ': endpoints)
  NoEndpoints :: Bundled '[]

class FlattenServer api where
  flattenServer :: Server api -> Bundled (Endpoints api)

instance
  ( FlattenServer api,
    Endpoints endpoint ~ '[endpoint]
  ) =>
  FlattenServer (endpoint :<|> api)
  where
  flattenServer (endpoint :<|> server) = endpoint `AnEndpoint` flattenServer @api server
 
instance
 (
   Endpoints api ~ '[api]
 ) =>
  FlattenServer (x :> api)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

instance FlattenServer (Verb method statusCode contentTypes responseType)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

class NormalizeFunction m where
  type Normal m
  normalize :: m -> Normal m

instance NormalizeFunction x => NormalizeFunction (r -> x) where
  type Normal (r -> x) = r -> Normal x
  normalize = fmap normalize
