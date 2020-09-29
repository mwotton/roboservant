{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import Hedgehog (Gen, Opaque, Var)
import qualified Hedgehog.Gen as Gen

type Stash v = Map TypeRep (NonEmpty (Var (Opaque (IORef Dynamic)) v))

-- class Typeable x => BuildFrom x where
--   buildFrom :: Stash -> [x]
--   default buildFrom :: Map TypeRep (NonEmpty Dynamic) -> [x]
--   buildFrom = maybe [] (fmap promisedDyn . NEL.toList) . Map.lookup (typeRep (Proxy @x))

-- instance BuildFrom x => BuildFrom (Maybe x) where
--   buildFrom dict = Nothing : fmap Just (buildFrom dict)

class Breakdown x where
  breakdown :: x -> NonEmpty Dynamic

-- | Can't break it down any further -- stuck in your teeth, maybe.
newtype Chewy x = Chewy x

instance Typeable x => Breakdown (Chewy x) where
  breakdown x = pure (toDyn x)

--let d = toDyn x in Map.fromList [(dynTypeRep d, pure d)]

-- instance (Typeable x, Generic x) => Breakdown x where
--   breakdown = Map.fromListWith (<>) . fmap ((dynTypeRep &&& (\x -> NEL.fromList [x])) . toDyn . Generics.to) . _ . Generics.from

class Create x where
  create :: Map TypeRep (NonEmpty Dynamic) -> Maybe (Gen x)

instance Typeable x => Create x where
  create mmm =
    (fmap Gen.element) $
      fmap promisedDyn . NEL.toList
        <$> Map.lookup (typeRep (Proxy @x)) mmm

-- | only use this when we are using the internal typerep map.
promisedDyn :: Typeable a => Dynamic -> a
promisedDyn = fromMaybe (error "internal error, typerep map misconstructed") . fromDynamic
