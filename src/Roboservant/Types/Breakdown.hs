{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics(Generic)
import Data.Typeable (Typeable)
import qualified Data.Dependent.Map as DM
import Data.Dependent.Map (DMap)
import qualified Type.Reflection as R
import Data.Dependent.Sum
import Data.Kind

data Provenance
  = Provenance R.SomeTypeRep Int
  deriving (Show,Eq)

class Breakdown x where
  breakdown :: x -> NonEmpty Dynamic

-- | Can't be built up from parts
newtype Atom x = Atom { unAtom :: x }

-- | can be broken down and built up from generic pieces
newtype Compound x = Compound { unCompound :: x }

-- | so, this is quite tricky. we need to witness the structure of `x`, but
--   we don't have an example of it yet. not entirely sure where to go.
instance (Typeable x, Generic x) => BuildFrom (Compound x) where
  buildFrom _stash = error "oops"
--   breakdown = Map.fromListWith (<>) . fmap ((dynTypeRep &&& (\x -> NEL.fromList [x])) . toDyn . Generics.to) . _ . Generics.from

-- -- | this should be a bit more tractable.
-- instance (Typeable x, Generic x) => Breakdown (Compound x) where
--   breakdown x = _ . Generics.from . unCompound $ x

instance Typeable x => BuildFrom (Atom x) where
  buildFrom = DM.lookup R.typeRep . getStash

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

-- | only use this when we are using the internal typerep map.
promisedDyn :: Typeable a => Dynamic -> a
promisedDyn = fromMaybe (error "internal error, typerep map misconstructed") . fromDynamic

deriving via (Atom Bool) instance BuildFrom Bool

instance (Typeable x, BuildFrom x) => BuildFrom (Maybe x) where
  buildFrom dict = Just options
    where options :: StashValue (Maybe x)
          options = StashValue $
            ([],Nothing) :|
              (maybe [] (NEL.toList . getStashValue . fmap Just) $ buildFrom @x dict
              )

deriving via (Atom ()) instance Breakdown ()
deriving via (Atom Int) instance Breakdown Int

instance Typeable a => Breakdown (Atom a) where
  breakdown = pure . toDyn . unAtom


instance (Typeable a, Breakdown a) => Breakdown [a] where
  breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)

instance (BuildFrom a) => BuildFrom [a] where
  buildFrom = error "fuuuck"
---  breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)
