{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}
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


class ToReifiedApi endpoints  m where
  toReifiedApi :: Bundled endpoints m -> Proxy endpoints -> ReifiedApi

instance ToReifiedApi '[] m where
  toReifiedApi NoEndpoints _ = []

instance
  ( NormalizeFunction (ServerT endpoint m)
  , Normal (ServerT endpoint m) ~ V.Curried (EndpointArgs endpoint) (IO (Either InteractionError (NonEmpty (Dynamic,Int))))
  , ToReifiedEndpoint endpoint
  , ToReifiedApi endpoints m
  ) =>
  ToReifiedApi (endpoint : endpoints) m
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
        renderServerError s = InteractionError (T.pack $ show s) (errHTTPCode serverError == 500)

    Right x -> pure $ Right $ breakdown x


--          case errHTTPCode serverError of
--            500 -> throw serverError
--            _ ->
--              liftIO . logInfo . show $ ("ignoring non-500 error", serverError)


data Bundled endpoints m where
  -- AnEndpoint :: Server endpoint -> Bundled endpoints -> Bundled (endpoint ': endpoints)
  AnEndpoint :: ServerT endpoint m -> Bundled endpoints m -> Bundled (endpoint ': endpoints) m
  NoEndpoints :: Bundled '[] m

class FlattenServer api m where
  flattenServer :: ServerT api m -> Bundled (Endpoints api) m

instance
  ( FlattenServer api m,
    Endpoints endpoint ~ '[endpoint]
  ) =>
  FlattenServer (endpoint :<|> api) m
  where
  flattenServer (endpoint :<|> server) = endpoint `AnEndpoint` flattenServer @api server

instance
 (
   Endpoints api ~ '[api]
 ) =>
  FlattenServer (x :> api) m
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

instance FlattenServer (Verb method statusCode contentTypes responseType) m
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

class NormalizeFunction m where
  type Normal m
  normalize :: m -> Normal m

instance NormalizeFunction x => NormalizeFunction (r -> x) where
  type Normal (r -> x) = r -> Normal x
  normalize = fmap normalize
