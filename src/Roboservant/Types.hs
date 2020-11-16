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
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Typeable (TypeRep)
import Hedgehog (HTraversable (..), Opaque, Symbolic, Var)
import Roboservant.Types.FlattenServer ( FlattenServer(..) )
import Roboservant.Types.ReifiedApi
    ( ApiOffset(..), ReifiedApi, ToReifiedApi(..) )

data State v
  = State
      { stateRefs :: Map TypeRep (NonEmpty (Var (Opaque Dynamic) v)),
      -- this is unfortunate. ideally we'd keep it structurally linked so there was a typerep
      -- for each var, but of course, when it's evaluated symbolically, there is no Var to look at.
      -- therefore we maintain the invariant manually
        chewable :: [(NonEmpty TypeRep,
            Var (Opaque (NonEmpty Dynamic)) v)  ]
      }

emptyState :: forall v. State v
emptyState = State mempty mempty

-- | we need to specify an offset because it's entirely possible to have two
--   functions with the same arguments that do different things.
data Op (v :: * -> *)
  = -- this needs to change - need a constructor offset and a list of vars inside,
    -- because a single argument to the api could be a sum type with many constructors and varying
    -- arguments itself.
    Op ApiOffset [Var (Opaque Dynamic) v]
  | Chewable Int TypeRep  (Var (Opaque (Dynamic)) v)

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r = \case
    Op offset args -> Op offset <$> traverse (htraverse r) args
    Chewable i trs v -> Chewable i trs <$> htraverse r v
