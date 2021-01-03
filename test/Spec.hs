{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Breakdown
import Data.Dynamic (toDyn)
import Data.Hashable (Hashable (hash))
import Data.Maybe (isNothing)
import Data.Void (Void)
import qualified Foo
import qualified Headers
import qualified Nested
import qualified Post
import qualified Product
import qualified Roboservant as RS
import qualified Seeded
import Test.Hspec
import Test.Hspec.Core.Spec (FailureReason (Reason), ResultStatus (Failure, Success), itemExample, mapSpecItem_, resultStatus)
import qualified Valid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "noError" $ do
      it "finds no error in a valid app" $ do
        RS.fuzz @Valid.Api Valid.server RS.defaultConfig
          >>= (`shouldSatisfy` isNothing)
      it "finds no error in a valid generic app" $ do
        RS.fuzz @Valid.RoutedApi Valid.routedServer RS.defaultConfig
          >>= (`shouldSatisfy` isNothing)
      it "does fail coverage check" $ do
        r <- RS.fuzz @Valid.Api Valid.server RS.defaultConfig {RS.coverageThreshold = 0.6}
        fmap (RS.failureReason . RS.rsException) r
          `shouldSatisfy` ( \case
                              Just (RS.InsufficientCoverage _) -> True
                              _ -> False
                          )
    describe "posted body" $ do
      it "passes a coverage check using a posted body" $ do
        RS.fuzz @Post.Api Post.server RS.defaultConfig {RS.coverageThreshold = 0.99}
          >>= (`shouldSatisfy` isNothing)
    describe "seeded" $ do
      shouldFail
        $ it "finds an error using information passed in"
        $ let res = Seeded.Seed 1
           in RS.fuzz @Seeded.Api Seeded.server (RS.defaultConfig {RS.seed = [(toDyn res, hash res)]})
                >>= (`shouldSatisfy` isNothing)
    describe "Foo" $ do
      it "finds an error in a basic app" $
        RS.fuzz @Foo.Api Foo.server RS.defaultConfig
          >>= (`shouldSatisfy` serverFailure)
  describe "BuildFrom" $ do
    describe "headers (and sum types)" $ do
      it "should find a failure that's dependent on using header info" $ do
        RS.fuzz @Headers.Api Headers.server RS.defaultConfig
          >>= (`shouldSatisfy` serverFailure)
    describe "product types" $ do
      it "should find a failure that's dependent on creating a product" $ do
        RS.fuzz @Product.Api Product.server RS.defaultConfig {RS.seed = [RS.hashedDyn 'a', RS.hashedDyn (1 :: Int)]}
          >>= (`shouldSatisfy` serverFailure)
  describe "Breakdown" $ do
    it "handles products" $ do
      RS.fuzz @Breakdown.ProductApi Breakdown.productServer RS.defaultConfig
        >>= (`shouldSatisfy` serverFailure)
    it "handles sums" $ do
      RS.fuzz @Breakdown.SumApi Breakdown.sumServer RS.defaultConfig
        >>= (`shouldSatisfy` serverFailure)
  describe "flattening" $ do
    -- we don't actually do much here, this is just here to document the appropriate response
    -- if you get a type error with a nested api.
    it "can handle nested apis" $ do
      RS.fuzz @(Nested.FlatApi) Nested.server RS.defaultConfig {RS.coverageThreshold = 0.99}
        >>= (`shouldSatisfy` isNothing)

serverFailure :: Maybe RS.Report -> Bool
serverFailure = \case
  Just RS.Report {..} ->
    let RS.RoboservantException {..} = rsException
     in failureReason /= RS.NoPossibleMoves
  _ -> False

deriving via (RS.Atom Foo.Foo) instance RS.Breakdown Foo.Foo

deriving via (RS.Atom Foo.Foo) instance RS.BuildFrom Foo.Foo

deriving via (RS.Atom Headers.Foo) instance RS.Breakdown Headers.Foo

deriving via (RS.Atom Headers.Foo) instance RS.BuildFrom Headers.Foo

deriving via (RS.Atom Seeded.Seed) instance RS.Breakdown Seeded.Seed

deriving via (RS.Atom Seeded.Seed) instance RS.BuildFrom Seeded.Seed

deriving via (RS.Atom Void) instance RS.BuildFrom Void

deriving via (RS.Atom Post.FooPost) instance RS.Breakdown Post.FooPost

deriving via (RS.Atom Post.FooPost) instance RS.BuildFrom Post.FooPost

deriving via (RS.Compound Breakdown.Foo) instance RS.Breakdown Breakdown.Foo

deriving via (RS.Compound Product.Foo) instance RS.BuildFrom Product.Foo

deriving via (RS.Compound Breakdown.SomeSum) instance RS.Breakdown Breakdown.SomeSum

-- | `shouldFail` allows you to assert that a given `Spec` should contain at least one failing test.
--   this is often useful when testing tests.
shouldFail :: SpecWith a -> SpecWith a
shouldFail =
  mapSpecItem_
    ( \i ->
        i
          { itemExample = \p a cb -> do
              r <- (itemExample i) p a cb
              pure
                r
                  { resultStatus = case resultStatus r of
                      Success -> Failure Nothing (Reason "Unexpected success")
                      Failure _ _ -> Success
                      x -> x
                  }
          }
    )
