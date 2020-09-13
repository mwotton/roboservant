{-# LANGUAGE LambdaCase #-}
module Roboservant.Hedgehog where

import           Hedgehog
import           Hedgehog.Gen
import qualified Hedgehog.Range as Range

-- | Uses a weighted distribution to randomly select one of the generators in
--   the list.
--
--   This generator shrinks towards the first generator in the list.
--
--   This is to be used when the generator returning a Nothing indicates
--   that it will never succeed so should be abandoned entirely.
--
--   /The input list must be non-empty./
--
frequencyM :: MonadGen m => [(Int, m (Maybe a))] -> m (Maybe a)
frequencyM = \case
  [] -> pure Nothing
    -- error "Hedgehog.Gen.frequency: used with empty list"
  xs0 -> do
    let
      pick acc n = \case
        [] -> pure Nothing
--          error "Hedgehog.Gen.frequency/pick: used with empty list"
        (k, x) : xs ->
          if n <= k then
            x >>= \case
              Nothing -> do
                -- this one will never come out right, so reassemble list without it
                frequencyM (acc <> xs)
              Just y -> pure $ Just y
          else
            pick ((k,x):acc) (n - k) xs

      total =
        sum (fmap fst xs0)

    n <- integral $ Range.constant 1 total
    pick [] n xs0
