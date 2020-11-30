{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingVia #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}


module Roboservant.Types.Breakdown where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import GHC.Generics(Generic)
import Data.Typeable (Typeable)
import Roboservant.Types.Internal
import Data.Hashable

-- | only use this when we are using the internal typerep map.
promisedDyn :: Typeable a => Dynamic -> a
promisedDyn = fromMaybe (error "internal error, typerep map misconstructed") . fromDynamic

class Breakdown x where
  breakdown :: x -> NonEmpty (Dynamic,Int)
--  default breakdown :: Typeable x => x -> NonEmpty Dynamic
--  breakdown = pure . toDyn


instance (Hashable x, Typeable x) => Breakdown (Atom x) where
  breakdown (Atom x) = pure (toDyn x, hash x)

deriving via (Atom ()) instance Breakdown ()
deriving via (Atom Int) instance Breakdown Int

-- instance (Typeable a, Breakdown a) => Breakdown [a] where
--   breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)
