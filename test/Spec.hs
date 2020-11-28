{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
import qualified Foo
import qualified Seeded
import qualified Valid
import qualified Headers
import qualified Post
import Test.Hspec.Core.Spec

import Data.Dynamic(toDyn)
import qualified Roboservant as RS
import Test.Hspec
import Data.Void
import Data.Maybe
import Data.Hashable

main :: IO ()
main = hspec spec

noCheck :: IO ()
noCheck = pure ()

defaultConfig :: RS.Config
defaultConfig = RS.Config
                { RS.seed = []
                , RS.maxRuntime = 0.5
                , RS.maxReps = 1000
                , RS.rngSeed = 0
                , RS.coverageThreshold = 0
                }

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "noError" $ do
      it "finds no error in a valid app" $ do
        RS.fuzz @Valid.Api Valid.server defaultConfig noCheck
          >>= (`shouldSatisfy` isNothing)
      it "does fail coverage check" $ do
        r <- RS.fuzz @Valid.Api Valid.server defaultConfig { RS.coverageThreshold = 0.6 } noCheck
        fmap (RS.failureReason . RS.rsException) r `shouldSatisfy`
          (\case
              Just (RS.InsufficientCoverage _) -> True
              _ -> False)

    describe "posted body" $ do
      it "passes a coverage check using a posted body" $ do
        RS.fuzz @Post.Api Post.server defaultConfig { RS.coverageThreshold = 0.99 } noCheck
          >>= (`shouldSatisfy` isNothing)

    describe "seeded" $ do
      shouldFail $
        it "finds an error using information passed in" $
          let res = Seeded.Seed 1 in
          RS.fuzz @Seeded.Api Seeded.server (defaultConfig{ RS.seed = [(toDyn res,hash res) ] }) noCheck
            >>= (`shouldSatisfy` isNothing)
    describe "Foo" $ do
      it "finds an error in a basic app" $
        RS.fuzz @Foo.Api Foo.server defaultConfig noCheck
          >>= (`shouldSatisfy` isJust)

    -- describe "headers" $ do
    --   it "should find a failure that's dependent on using header info" $ do
    --     RS.fuzz @Headers.Api Headers.server defaultConfig noCheck
    --       >>= (`shouldSatisfy` isJust)

    -- describe "can build from pieces" $ do
    --   it "should find a failure that requires some assembly" $ do
    --     RS.fuzz @RS.BuildFrom.Api RS.BuildFrom.server defaultConfig noCheck
    --       >>= (`shouldSatisfy` isJust)



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


deriving via (RS.Atom Foo.Foo) instance RS.Breakdown Foo.Foo
deriving via (RS.Atom Foo.Foo) instance RS.BuildFrom Foo.Foo

deriving via (RS.Atom Headers.Foo) instance RS.Breakdown Headers.Foo
deriving via (RS.Atom Headers.Foo) instance RS.BuildFrom Headers.Foo

deriving via (RS.Atom Seeded.Seed) instance RS.Breakdown Seeded.Seed
deriving via (RS.Atom Seeded.Seed) instance RS.BuildFrom Seeded.Seed


-- instance RS.BuildFrom Seeded.Seed

deriving via (RS.Atom Void) instance RS.BuildFrom Void

deriving via (RS.Atom Post.FooPost) instance RS.BuildFrom Post.FooPost
deriving via (RS.Atom Post.FooPost) instance RS.Breakdown Post.FooPost


--deriving via (Compound RS.BuildFrom.Wrapped) instance RS.BuildFrom RS.BuildFrom.Wrapped
--deriving via (Compound RS.BuildFrom.Wrapped) instance RS.Breakdown RS.BuildFrom.Wrapped



-- deriving via (RS.Atom Void) instance RS.BuildFrom Void

-- instance RS.Breakdown Post.FooPost
-- instance RS.BuildFrom Post.FooPost


-- | `shouldFail` allows you to assert that a given `Spec` should contain at least one failing test.
--   this is often useful when testing tests.
shouldFail :: SpecWith a -> SpecWith a
shouldFail = mapSpecItem_ (\i -> i {
                              itemExample = \p a cb -> do
                                  r <- (itemExample i) p a cb
                                  pure r {resultStatus = case resultStatus r of
                                             Success -> Failure Nothing (Reason "Unexpected success")
                                             Failure _ _ -> Success
                                             x -> x
                                         }
                                   })
