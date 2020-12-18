

{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Hashable
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.Maybe(catMaybes,fromMaybe)

buildFrom :: forall x . (Hashable x, BuildFrom x, Typeable x) => Stash -> Maybe (StashValue x)
buildFrom stash = buildStash (maybe [] (NEL.toList . getStashValue)  (DM.lookup R.typeRep (getStash stash))
                              <> extras stash )
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

--        addToHash :: x -> IntSet -> IntSet
--        addToHash x = IntSet.insert

class (Hashable x, Typeable x) => BuildFrom (x :: Type) where
  extras :: Stash -> [([Provenance], x)]

instance (Hashable x, Typeable x) => BuildFrom (Atom x) where
  extras _ = []

deriving via (Atom Bool) instance BuildFrom Bool

-- instance (Typeable x, Hashable x, BuildFrom x) => BuildFrom (Maybe x) where
--  extras dict = _


-- collapseList :: (Hashable a, Typeable a) => [a] -> Maybe (StashValue a)
-- collapseList = fmap buildStashValue . NE.nonEmpty --  . filter jsonParseable
--   where
--     -- this is... not particularly principled. the provenance stuff probably needs to be reworked.
--     buildStashValue values = StashValue
--       { getStashValue = fmap ([],) values
--       , stashHash = IntSet.fromList $ hash <$> NE.toList values
--       }
-- -- instance (BuildFrom a) => BuildFrom [a] -- where
--   --breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)


-- instance (Typeable x, Hashable x, BuildFrom x) => BuildFrom (Maybe x) where
--   buildFrom dict = Just options
--     where options :: StashValue (Maybe x)
--           options = StashValue
--             ([],Nothing) :|
--               (maybe [] (NEL.toList . getStashValue . fmap Just) $ buildFrom @x dict
--               )
--             _
-- -- instance (BuildFrom a) => BuildFrom [a] -- where
--   --breakdown x = toDyn x :| mconcat (map (NEL.toList . breakdown) x)
