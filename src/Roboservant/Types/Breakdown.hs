{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

-- | only use this when we are using the internal typerep map.
promisedDyn :: Typeable a => Dynamic -> a
promisedDyn = fromMaybe (error "internal error, typerep map misconstructed") . fromDynamic

class Breakdown x where
  breakdown :: x -> NonEmpty Dynamic
  default breakdown :: Typeable x => x -> NonEmpty Dynamic
  breakdown = pure . toDyn

-- | Can't break it down any further -- stuck in your teeth, maybe.
newtype Chewy x = Chewy {unChew :: x}

instance Breakdown () where
  breakdown = pure . toDyn

instance Breakdown Int where
  breakdown = pure . toDyn

instance Typeable x => Breakdown (Chewy x) where
  breakdown x = pure (toDyn x)

instance (Typeable a, Breakdown a) => Breakdown [a] where
  breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)
