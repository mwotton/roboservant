
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Foo
import qualified Seeded
import qualified Valid
import qualified Headers
import qualified UnsafeIO
import qualified Roboservant as RS

-- import Test.Hspec.Hedgehog
import Test.Hspec
import Test.Hspec.Core.Spec(shouldFail)

import Data.Dynamic(toDyn)

main :: IO ()
main = hspec spec

noCheck = pure ()

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "noError" $ do
      it "finds no error in a valid app" $ do
        RS.fuzz @Valid.Api Valid.server (RS.Config [toDyn $ Seeded.Seed 1] 1 1000 1) noCheck
    describe "seeded" $ do
      shouldFail $
        it "finds an error using information passed in" $
          RS.fuzz @Seeded.Api Seeded.server (RS.Config [toDyn $ Seeded.Seed 1] 5 1000 1) noCheck

      shouldFail $ it "finds an error in a basic app" $
        RS.fuzz @Foo.Api Foo.server (RS.Config [] 5 1000 1) noCheck

      shouldFail $ it "should find a failure that's dependent on using header info" $ do
        RS.fuzz @Headers.Api Headers.server (RS.Config [] 5 10000 1) noCheck

  -- -- -- The UnsafeIO checker does not actually really use the contextually aware stuff, though it
  -- -- -- could: it's mostly here to show how to test for concurrency problems.
  -- describe "concurrency bugs" $ do
  --   before UnsafeIO.makeServer $ do
  --     describe "sequential checking" $ do
  --       it "safe use" $ \unsafeServer -> do
  --         hedgehog $ RS.prop_sequential @UnsafeIO.UnsafeApi unsafeServer []

  --     modifyMaxSuccess (const 10000) $
  --       shouldFail $
  --         describe "concurrent" $ do
  --           it "concurrent, dangerous use" $ \unsafeServer -> do
  --             RS.prop_concurrent @UnsafeIO.UnsafeApi unsafeServer []


instance RS.Breakdown Foo.Foo where
  breakdown = pure . toDyn


instance RS.Breakdown Headers.Foo where
  breakdown = pure . toDyn

instance RS.BuildFrom Foo.Foo

instance RS.BuildFrom Headers.Foo

instance RS.BuildFrom Seeded.Seed
