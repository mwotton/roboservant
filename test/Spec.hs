
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Foo
import qualified Seeded
import qualified Headers
import qualified UnsafeIO
import qualified Roboservant as RS

import Test.Hspec.Hedgehog
import Test.Hspec
import Test.Hspec.Core.Spec(shouldFail)

import Data.Dynamic(toDyn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "seeded" $ do
      modifyMaxSuccess (const 10000) $
        shouldFail $
          it "finds an error using information passed in" $
            hedgehog $ RS.prop_sequential @Seeded.Api Seeded.server [toDyn $ Seeded.Seed 1]

    modifyMaxSuccess (const 10000) $

      shouldFail $ it "finds an error in a basic app" $
        hedgehog $ RS.prop_sequential @Foo.Api Foo.server []

  describe "Headers" $ do
    shouldFail $ it "should find a failure that's dependent on using header info" $ do
      let _ = hedgehog $ RS.prop_sequential @Headers.Api Headers.server []
      pending

  -- -- The UnsafeIO checker does not actually really use the contextually aware stuff, though it
  -- -- could: it's mostly here to show how to test for concurrency problems.
  describe "concurrency bugs" $ do
    before UnsafeIO.makeServer $ do
      describe "sequential checking" $ do
        it "safe use" $ \unsafeServer -> do
          hedgehog $ RS.prop_sequential @UnsafeIO.UnsafeApi unsafeServer []

      modifyMaxSuccess (const 10000) $
        shouldFail $
          describe "concurrent" $ do
            it "concurrent, dangerous use" $ \unsafeServer -> do
              RS.prop_concurrent @UnsafeIO.UnsafeApi unsafeServer []
