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
      { stateRefs :: Map TypeRep (NonEmpty (Var (Opaque (IORef Dynamic)) v)),
        preloads :: [Dynamic]
      }

-- | we need to specify an offset because it's entirely possible to have two
--   functions with the same arguments that do different things.
data Op (v :: * -> *)
  = -- this needs to change - need a constructor offset and a list of vars inside,
    -- because a single argument to the api could be a sum type with many constructors and varying
    -- arguments itself.
    Op ApiOffset [(Var (Opaque (IORef Dynamic)) v)]
  | Preload TypeRep Dynamic

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r = \case
    Op offset args -> Op offset <$> traverse (htraverse r) args
    Preload tr v -> pure $ Preload tr v
