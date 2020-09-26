{-# LANGUAGE LambdaCase #-}

module Roboservant.Hedgehog where
import Hedgehog
import qualified Hedgehog.Gen as Gen

elementOrFail :: MonadGen m => [a] -> m a
elementOrFail = \case
  [] -> Gen.discard
  x -> Gen.element x
