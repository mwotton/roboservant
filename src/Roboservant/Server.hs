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
import Servant (Endpoints, Proxy (Proxy), ServerT)
import Roboservant.Types.Config

fuzz :: forall api m.
              (FlattenServer api m, ToReifiedApi (Endpoints api) m) =>
              ServerT api m ->
              Config ->
              IO (Maybe Report)
fuzz s  = fuzz' (reifyServer s)
  -- todo: how do we pull reifyServer out?
  where reifyServer :: (FlattenServer api m, ToReifiedApi (Endpoints api) m)
                    => ServerT api m -> ReifiedApi
        reifyServer server = toReifiedApi (flattenServer @api @m server) (Proxy @(Endpoints api))
--        reifyServer server = toReifiedApi server (Proxy @(Endpoints api))

