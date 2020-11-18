{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import qualified Data.Dependent.Map as DM
import Data.Dependent.Map (DMap)
import qualified Type.Reflection as R
import Data.Dependent.Sum
import Data.Kind

data Provenance
  = Provenance R.SomeTypeRep Int
  deriving (Show,Eq)

newtype StashValue a = StashValue { getStashValue :: NonEmpty ([Provenance], a) }
  deriving (Functor, Show)

-- wrap in newtype to give a custom Show instance, since the normal
-- instance for DMap is not happy since StashValue needs Show a to show
newtype Stash = Stash { getStash :: DMap R.TypeRep StashValue }
  deriving (Semigroup, Monoid)

instance Show Stash where
    showsPrec i (Stash x) = showsPrec i $
      Map.fromList . map (\(tr :=> StashValue vs) -> (R.SomeTypeRep tr, fmap fst vs)) $ DM.toList x

class Typeable x => BuildFrom (x :: Type) where
  buildFrom :: Stash -> Maybe (StashValue x)
  default buildFrom :: Stash -> Maybe (StashValue x)
  buildFrom = DM.lookup R.typeRep . getStash

  -- (fmap promisedDyn . NEL.toList) . Map.lookup (typeRep (Proxy @x))


-- baseLookup :: TypeRep -> Stash -> Maybe (NonEmpty ([Provenance], Dynamic))
-- baseLookup tr mmm = -- Map.lookup (typeRep (Proxy @x)) mmm
--   Map.lookup tr mmm

-- | only use this when we are using the internal typerep map.
promisedDyn :: Typeable a => Dynamic -> a
promisedDyn = fromMaybe (error "internal error, typerep map misconstructed") . fromDynamic

instance BuildFrom Bool

instance (Typeable x, BuildFrom x) => BuildFrom (Maybe x) where
  buildFrom dict = Just options
    where options :: StashValue (Maybe x)
          options = StashValue $
            ([],Nothing) :|
              (maybe [] (NEL.toList . getStashValue . fmap Just) $ buildFrom @x dict
              )

class Breakdown x where
  breakdown :: x -> NonEmpty Dynamic
  default breakdown :: Typeable x => x -> NonEmpty Dynamic
  breakdown = pure . toDyn
  
-- | Can't break it down any further -- stuck in your teeth, maybe.
newtype Chewy x = Chewy { unChew :: x }

instance Breakdown () where
  breakdown = pure . toDyn

instance Breakdown Int where
  breakdown = pure . toDyn


instance Typeable x => Breakdown (Chewy x) where
  breakdown x = pure (toDyn x)

--let d = toDyn x in Map.fromList [(dynTypeRep d, pure d)]

-- instance (Typeable x, Generic x) => Breakdown x where
--   breakdown = Map.fromListWith (<>) . fmap ((dynTypeRep &&& (\x -> NEL.fromList [x])) . toDyn . Generics.to) . _ . Generics.from


instance (Typeable a, Breakdown a) => Breakdown [a] where
  breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)

instance (BuildFrom a) => BuildFrom [a] -- where
--  breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)

