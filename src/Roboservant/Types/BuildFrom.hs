{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Roboservant.Types.BuildFrom where

import Control.Monad(filterM)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Typeable (Typeable)
import qualified Data.Dependent.Map as DM
import qualified Type.Reflection as R
import Data.Kind
import Roboservant.Types.Internal
import Data.Hashable
import qualified Data.IntSet as IntSet
import GHC.Generics

buildFrom :: forall x . (Hashable x, BuildFrom x, Typeable x) => Stash -> Maybe (StashValue x)
buildFrom = buildStash . buildFrom'
  where
    buildStash :: [([Provenance], x)] -> Maybe (StashValue x)
    buildStash  = fmap (foldr1 addStash . fmap promoteToStash) . NEL.nonEmpty

    promoteToStash :: ([Provenance], x) -> StashValue x
    promoteToStash (p,x) = StashValue (pure (p,x))
                             (IntSet.singleton (hash x))

    -- | sorta fiddly. looks like (<>), but in general it's not safe to add arbitrary StashValues, because
    --   there's no guarantee that they came from the same run, which would invalidate all the provenance stuff.
    --   in this context it's safe because we have no access to anything else.
    --
    --   we _do_ have to check here that the new elements are not already contained, however.
    addStash :: StashValue x -> StashValue x -> StashValue x
    addStash old (StashValue newVal _)
      = let insertableVals = NEL.filter ((`IntSet.notMember` stashHash old) . hash) newVal
        in StashValue ( addListToNE (getStashValue old) insertableVals )
           (IntSet.union (IntSet.fromList . map hash . fmap snd . NEL.toList $ newVal) (stashHash old))

    addListToNE :: NonEmpty a -> [a] -> NonEmpty a
    addListToNE ne l = NEL.fromList (NEL.toList ne <> l)

buildFrom' :: forall x . (Hashable x, BuildFrom x, Typeable x) => Stash -> [([Provenance], x)]
buildFrom' stash = maybe [] (NEL.toList . getStashValue)  (DM.lookup R.typeRep (getStash stash))
                   <> extras stash

class (Hashable x, Typeable x) => BuildFrom (x :: Type) where
  extras :: Stash -> [([Provenance], x)]

instance (Hashable x, Typeable x) => BuildFrom (Atom x) where
  extras _ = []

deriving via (Atom Bool) instance BuildFrom Bool

deriving via (Compound (Maybe x)) instance (Typeable x, Hashable x, BuildFrom x) => BuildFrom (Maybe x)

-- this isn't wonderful, but we need a hand-rolled instance for recursive datatypes right now.
-- with an arbitrary-ish interface, we could use a size parameter, rng access etc.
instance (BuildFrom x) => BuildFrom [x] where
  extras stash =  map (\xs -> (concatMap fst xs,map snd xs)) $ powerset $ extras @x stash
    where
      powerset xs = filterM (const [True, False]) xs

instance (Hashable x, Typeable x, Generic x, GBuildFrom (Rep x)) => BuildFrom (Compound (x::Type)) where
  extras stash = fmap (Compound . to) <$> gExtras stash

deriving via (Atom Int) instance BuildFrom Int
deriving via (Atom Char) instance BuildFrom Char

class GBuildFrom (f :: k -> *)  where
  gExtras :: Stash -> [([Provenance], f a)]

instance GBuildFrom b => GBuildFrom (M1 D a b) where
  gExtras = fmap (fmap M1) . gExtras

-- not recursion safe!
instance (GBuildFrom a, GBuildFrom b) => GBuildFrom (a :+: b) where
  gExtras stash = (fmap L1 <$> gExtras stash)
               <> (fmap R1 <$> gExtras stash)

instance (GBuildFrom a, GBuildFrom b) => GBuildFrom (a :*: b) where
  gExtras stash = [ (pa<>pb, a' :*: b')  | (pa,a') <- gExtras stash , (pb,b') <- gExtras stash]

instance GBuildFrom b => GBuildFrom (M1 C a b) where
  gExtras =fmap (fmap M1) . gExtras

instance GBuildFrom b => GBuildFrom (M1 S a b) where
  gExtras = fmap (fmap M1) . gExtras

instance BuildFrom a => GBuildFrom (K1 i a) where
  gExtras = fmap (fmap K1) . buildFrom'

instance GBuildFrom U1 where
  gExtras _ = [([],U1)]
