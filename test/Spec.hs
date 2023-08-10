{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Breakdown
import Data.Dynamic (toDyn)
import Data.Hashable (Hashable (hash))
import Data.Maybe (isNothing)
import Data.Void (Void)
import qualified Foo
import qualified Headers
import qualified Nested
import qualified Post
import qualified Put
import qualified Product
import qualified QueryParams
import qualified NamedRoute
import qualified Roboservant as R
import qualified Roboservant.Server as RS
import qualified Roboservant.Client as RC
import qualified Seeded
import Test.Hspec
import Test.Hspec.Core.Spec (FailureReason (Reason), ResultStatus (Failure, Success), itemExample, mapSpecItem_, resultStatus)
import qualified Valid
import Servant ( Server, Get, JSON, Proxy(..), serve, Endpoints, HasServer )

import Servant.Client(ClientEnv, mkClientEnv, baseUrlPort, parseBaseUrl,HasClient,ClientM,client)
import Network.Wai(Application)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client       (newManager, defaultManagerSettings)
import Control.Monad((>=>))
import Control.Monad.Trans.Reader (runReaderT)

main :: IO ()
main = hspec spec


fuzzBoth
  :: forall a .
     (R.ToReifiedApi (Endpoints a), HasServer a '[], RS.FlattenServer a, RC.ToReifiedClientApi (Endpoints a), RC.FlattenClient a,
     HasClient ClientM a)
  => String -> Server a -> R.Config -> (Maybe R.Report -> IO ()) -> Spec
fuzzBoth name server config condition = do
  it (name <> " via server") $
    RS.fuzz @a server config >>= condition

  around (withServer (serve (Proxy :: Proxy a) server)) $
    it (name <> " via client") $ \(clientEnv::ClientEnv) -> do
    RC.fuzz @a clientEnv config >>= condition


fuzzClient
  :: forall a .
     (R.ToReifiedApi (Endpoints a), HasServer a '[],  RC.ToReifiedClientApi (Endpoints a), RC.FlattenClient a,
     HasClient ClientM a)
  => String -> Server a -> R.Config -> (Maybe R.Report -> IO ()) -> Spec
fuzzClient name server config condition = do
  around (withServer (serve (Proxy :: Proxy a) server)) $
    it (name <> " via client") $ \(clientEnv::ClientEnv) -> do
    RC.fuzz @a clientEnv config >>= condition


withServer :: Application -> ActionWith ClientEnv -> IO ()
withServer app action = Warp.testWithApplication (pure app) (genClientEnv >=> action)
  where genClientEnv port = do
          baseUrl <- parseBaseUrl "http://localhost"
          manager <- newManager defaultManagerSettings
          pure $ mkClientEnv manager (baseUrl { baseUrlPort = port })

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "noError" $ do
      fuzzBoth @Valid.Api "find no error in a basic app" Valid.server R.defaultConfig (`shouldSatisfy` isNothing)
      fuzzBoth @Valid.RoutedApi "finds no error in a valid generic app"   Valid.routedServer R.defaultConfig (`shouldSatisfy` isNothing)
      fuzzBoth @Valid.Api "fails coverage check" Valid.server R.defaultConfig {R.coverageThreshold = 0.6}
        (\r ->
           fmap (R.failureReason . R.rsException) r
           `shouldSatisfy` ( \case
                               Just (R.InsufficientCoverage _) -> True
                               _ -> False
                           ))
    describe "posted body" $
      fuzzBoth @Post.Api "passes a coverage check using a posted body" Post.server R.defaultConfig {R.coverageThreshold = 0.99}
      (`shouldSatisfy` isNothing)


    describe "PUTted body" $
      fuzzBoth @Put.Api "passes a coverage check using a posted body" Put.server R.defaultConfig {R.coverageThreshold = 0.99}
      (`shouldSatisfy` isNothing)


    describe "seeded" $ do
      let res = Seeded.Seed 1
      shouldFail $ fuzzBoth @Seeded.Api "finds an error using information passed in" Seeded.server
        (R.defaultConfig {R.seed = [(toDyn res, hash res)]})
        (`shouldSatisfy` isNothing)

    describe "Foo" $
      fuzzBoth @Foo.Api "finds an error in a basic app" Foo.server R.defaultConfig (`shouldSatisfy` serverFailure)

    describe "QueryParams" $
      fuzzBoth @QueryParams.Api "can handle query params" QueryParams.server R.defaultConfig { R.seed = [R.hashedDyn (12::Int)] }
      (`shouldSatisfy` isNothing)

  describe "BuildFrom" $ do
    describe "headers (and sum types)" $
      fuzzBoth @Headers.Api "should find a failure that's dependent on using header info" Headers.server R.defaultConfig
      (`shouldSatisfy` serverFailure)
    describe "product types" $
      fuzzBoth @Product.Api "should find a failure that's dependent on creating a product" Product.server
      R.defaultConfig {R.seed = [R.hashedDyn 'a', R.hashedDyn (1 :: Int)]}
      (`shouldSatisfy` serverFailure)
  describe "Breakdown" $ do
    fuzzBoth @Breakdown.ProductApi "handles products"  Breakdown.productServer R.defaultConfig
      (`shouldSatisfy` serverFailure)
    fuzzBoth @Breakdown.SumApi "handles sums" Breakdown.sumServer R.defaultConfig
      (`shouldSatisfy` serverFailure)
  describe "flattening" $
    fuzzBoth @Nested.FlatApi "can handle nested apis" Nested.server R.defaultConfig {R.coverageThreshold = 0.9999999}
    (`shouldSatisfy` isNothing)
  describe "NamedRoute" $
    fuzzClient @NamedRoute.API "can handle named routes" NamedRoute.server R.defaultConfig {R.coverageThreshold = 0.99}
    (`shouldSatisfy` isNothing)
  it "NormalizeFunction" $ do
    let mockClient :: ClientM Int
        mockClient = client (Proxy :: Proxy (Get '[JSON] Int))
    let normalizedClient = R.normalize mockClient
    result <- runReaderT normalizedClient _
    fmap (fmap snd) result `shouldBe` _

serverFailure :: Maybe R.Report -> Bool
serverFailure = \case
  Just R.Report {..} ->
    let R.RoboservantException {..} = rsException
     in failureReason /= R.NoPossibleMoves
  _ -> False

deriving via (R.Atom Foo.Foo) instance R.Breakdown Foo.Foo

deriving via (R.Atom Foo.Foo) instance R.BuildFrom Foo.Foo

deriving via (R.Atom Headers.Foo) instance R.Breakdown Headers.Foo

deriving via (R.Atom Headers.Foo) instance R.BuildFrom Headers.Foo

deriving via (R.Atom Seeded.Seed) instance R.Breakdown Seeded.Seed

deriving via (R.Atom Seeded.Seed) instance R.BuildFrom Seeded.Seed

deriving via (R.Atom Void) instance R.BuildFrom Void

deriving via (R.Atom Post.FooPost) instance R.Breakdown Post.FooPost
deriving via (R.Atom Post.FooPost) instance R.BuildFrom Post.FooPost

deriving via (R.Atom Put.Foo) instance R.Breakdown Put.Foo
deriving via (R.Atom Put.Foo) instance R.BuildFrom Put.Foo



deriving via (R.Compound Breakdown.Foo) instance R.Breakdown Breakdown.Foo

deriving via (R.Compound Product.Foo) instance R.BuildFrom Product.Foo

deriving via (R.Compound Breakdown.SomeSum) instance R.Breakdown Breakdown.SomeSum

-- | `shouldFail` allows you to assert that a given `Spec` should contain at least one failing test.
--   this is often useful when testing tests.
shouldFail :: SpecWith a -> SpecWith a
shouldFail =
  mapSpecItem_
    ( \i ->
        i
          { itemExample = \p a cb -> do
              r <- itemExample i p a cb
              pure
                r
                  { resultStatus = case resultStatus r of
                      Success -> Failure Nothing (Reason "Unexpected success")
                      Failure _ _ -> Success
                      x -> x
                  }
          }
    )
