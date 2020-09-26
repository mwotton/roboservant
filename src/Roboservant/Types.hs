{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Types
  ( module Roboservant.Types,
    FlattenServer (..),
    ToReifiedApi (..),
    ReifiedApi,
    ApiOffset (..),
  )
where

import Data.Dynamic (Dynamic)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Typeable (TypeRep)
import Hedgehog (HTraversable (..), Opaque, Symbolic, Var)
import Roboservant.Types.FlattenServer
import Roboservant.Types.ReifiedApi

data State v
  = State
      { stateRefs :: Map TypeRep (NonEmpty (Var (Opaque (IORef Dynamic)) v))
      }

-- | we need to specify an offset because it's entirely possible to have two
--   functions with the same arguments that do different things.
data Op (v :: * -> *)
  = Op ApiOffset [(TypeRep, Var (Opaque (IORef Dynamic)) v)]
  | Preload TypeRep (Var Dynamic v)

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r = \case
    Op offset args -> Op offset <$> traverse (\(t, v) -> (t,) <$> htraverse r v) args
    Preload tr v -> Preload tr <$> htraverse r v
