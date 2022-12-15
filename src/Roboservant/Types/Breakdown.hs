{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic)
import Data.Hashable
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Typeable (Typeable)
import GHC.Generics
import Roboservant.Types.Internal
import Servant
import Roboservant.Types.Orphans()

breakdown ::
  (Hashable x, Typeable x, Breakdown x) =>
  x ->
  NonEmpty (Dynamic, Int)
breakdown x = hashedDyn x :| breakdownExtras x

class Breakdown x where
  breakdownExtras :: x -> [(Dynamic, Int)]

instance (Hashable x, Typeable x) => Breakdown (Atom x) where
  breakdownExtras _ = []

deriving via (Atom ()) instance Breakdown ()

deriving via (Atom Int) instance Breakdown Int
deriving via (Atom Char) instance Breakdown Char

deriving via (Compound (Maybe x)) instance (Typeable x, Hashable x, Breakdown x) => Breakdown (Maybe x)

instance (Hashable x, Typeable x, Breakdown x) => Breakdown [x] where
  breakdownExtras stash =  concatMap (NEL.toList . breakdown) stash


class GBreakdown (f :: k -> Type) where
  gBreakdownExtras :: f a -> [(Dynamic, Int)]

instance (Hashable x, Typeable x, Generic x, GBreakdown (Rep x)) => Breakdown (Compound (x :: Type)) where
  breakdownExtras = gBreakdownExtras . from . unCompound

instance GBreakdown f => GBreakdown (M1 S c f) where
  gBreakdownExtras (M1 f) = gBreakdownExtras f

instance GBreakdown b => GBreakdown (M1 D a b) where
  gBreakdownExtras (M1 f) = gBreakdownExtras f

instance GBreakdown b => GBreakdown (M1 C a b) where
  gBreakdownExtras (M1 f) = gBreakdownExtras f

instance (GBreakdown a, GBreakdown b) => GBreakdown (a :*: b) where
  gBreakdownExtras (a :*: b) = gBreakdownExtras a <> gBreakdownExtras b

instance (GBreakdown a, GBreakdown b) => GBreakdown (a :+: b) where
  gBreakdownExtras = \case
    L1 a -> gBreakdownExtras a
    R1 a -> gBreakdownExtras a

instance (Hashable a, Typeable a, Breakdown a) => GBreakdown (K1 R a) where
  gBreakdownExtras (K1 c) = NEL.toList $ breakdown c

instance GBreakdown U1 where
  gBreakdownExtras U1 = []

deriving via (Atom NoContent) instance Breakdown NoContent
