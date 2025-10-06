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

module Roboservant.Server
  ( fuzz,
    fuzzProperty,
    fuzzTestCase,
    module Roboservant.Types
  )
where

import Minithesis (Property, TestCase)
import qualified Roboservant.Direct as RD
import Roboservant.Direct (Report)
import Roboservant.Types
  ( FlattenServer (..),
    ReifiedApi,
  )
import Roboservant.Types.Config
import Roboservant.Types.ReifiedApi.Server (ToReifiedApi (..))
import Servant (Endpoints, Proxy (Proxy), Server)

fuzz ::
  forall api.
  (FlattenServer api, ToReifiedApi (Endpoints api)) =>
  Server api ->
  Config ->
  IO (Maybe Report)
fuzz server = RD.fuzz' (reifyServer @api server)

fuzzProperty ::
  forall api.
  (FlattenServer api, ToReifiedApi (Endpoints api)) =>
  Server api ->
  Config ->
  Property
fuzzProperty server = RD.fuzzProperty (reifyServer @api server)

fuzzTestCase ::
  forall api.
  (FlattenServer api, ToReifiedApi (Endpoints api)) =>
  Server api ->
  Config ->
  TestCase ->
  IO (Maybe Report)
fuzzTestCase server = RD.runFuzzTestCase (reifyServer @api server)

reifyServer ::
  forall api.
  (FlattenServer api, ToReifiedApi (Endpoints api)) =>
  Server api ->
  ReifiedApi
reifyServer server =
  toReifiedApi
    (flattenServer @api server)
    (Proxy @(Endpoints api))
