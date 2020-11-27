{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}



module Roboservant.Types.BuildFrom where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Typeable (Typeable)
import qualified Data.Dependent.Map as DM
import qualified Type.Reflection as R
import Data.Kind
import Roboservant.Types.Internal

class Typeable x => BuildFrom (x :: Type) where
  buildFrom :: Stash -> Maybe (StashValue x)

-- | this is a little esoteric: basically, we're always going to use this with
--   deriving-via, so we need to look up the underlying `x` and then wrap it in Atom.
instance Typeable x => BuildFrom (Atom x) where
  buildFrom = fmap (fmap Atom) <$> DM.lookup R.typeRep . getStash

deriving via (Atom Bool) instance BuildFrom Bool

instance (Typeable x, BuildFrom x) => BuildFrom (Maybe x) where
  buildFrom dict = Just options
    where options :: StashValue (Maybe x)
          options = StashValue $
            ([],Nothing) :|
              (maybe [] (NEL.toList . getStashValue . fmap Just) $ buildFrom @x dict
              )
-- instance (BuildFrom a) => BuildFrom [a] -- where
  --breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)
