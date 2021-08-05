{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Server (fuzz, module Roboservant.Types ) where

import Roboservant.Direct(fuzz',Report)
import Roboservant.Types
  ( FlattenServer (..),
    ReifiedApi,
  )
import Roboservant.Types.ReifiedApi.Server(ToReifiedApi (..))
import Servant (Endpoints, Proxy (Proxy), Server)
import Roboservant.Types.Config

fuzz :: forall api.
              (FlattenServer api, ToReifiedApi (Endpoints api)) =>
              Server api ->
              Config ->
              IO (Maybe Report)
fuzz s  = fuzz' (reifyServer s)
  -- todo: how do we pull reifyServer out?
  where reifyServer :: (FlattenServer api, ToReifiedApi (Endpoints api))
                    => Server api -> ReifiedApi
        reifyServer server = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
--        reifyServer server = toReifiedApi server (Proxy @(Endpoints api))

