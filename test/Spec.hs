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
import qualified Data.Dependent.Map as DM
import Data.Dynamic (toDyn)
import Data.Hashable (Hashable (hash))
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Foo
import qualified Headers
import qualified Nested
import qualified Post
import qualified Put
import qualified Product
import qualified QueryParams
import qualified MinithesisExample
import qualified MinithesisAuth
import qualified Records
import qualified Roboservant as R
import qualified Roboservant.Server as RS
import qualified Roboservant.Client as RC
import qualified Seeded
import Test.Syd
import qualified Valid
import Servant ( Server, Proxy(..), serve, Endpoints, HasServer )

import Servant.Client(ClientEnv, mkClientEnv, baseUrlPort, parseBaseUrl,HasClient,ClientM)
import Network.Wai(Application)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client       (Manager, defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import qualified Network.Socket as Socket
import qualified Type.Reflection as TR
import System.IO (stdout, stderr)
import System.IO.Silently (hCapture)

newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 1000000 }

main :: IO ()
main = sydTest spec

fuzzBoth
  :: forall a .
     (R.ToReifiedApi (Endpoints a), HasServer a '[], RS.FlattenServer a, RC.ToReifiedClientApi (Endpoints a), RC.FlattenClient a,
     HasClient ClientM a)
  => String -> Server a -> R.Config -> (Maybe R.Report -> IO ()) -> Spec
fuzzBoth name server config condition = do
  it (name <> " via server") $
    do
      (logs, report) <-
        hCapture [stdout, stderr] $ RS.fuzz @a server config
      context ("captured server logs:\n" <> logs) $
        condition report

  around (withServer (serve (Proxy :: Proxy a) server)) $
    it (name <> " via client") $ \(clientEnv::ClientEnv) -> do
    RC.fuzz @a clientEnv config >>= condition

withServer :: Application -> ((ClientEnv -> IO ()) -> IO ())
withServer app inner = Warp.testWithApplication (pure app) $ \port -> do
  env <- genClientEnv port
  inner env
  where genClientEnv port = do
          baseUrl <- parseBaseUrl "http://localhost"
          manager <- newTestManager
          pure $ mkClientEnv manager (baseUrl { baseUrlPort = port })

withClosedPort :: ((ClientEnv -> IO ()) -> IO ())
withClosedPort inner = do
  (port, sock) <- Warp.openFreePort
  Socket.close sock
  manager <- newTestManager
  baseUrl <- parseBaseUrl ("http://127.0.0.1:" <> show port)
  let env = mkClientEnv manager baseUrl
  inner env

spec :: Spec
spec = do
  describe "Basic usage" $ do
    describe "noError" $ do
      fuzzBoth @Valid.Api "find no error in a basic app" Valid.server R.defaultConfig (`shouldSatisfy` isNothing)
      fuzzBoth @Valid.RoutedApi "finds no error in a valid generic app"   Valid.routedServer R.defaultConfig (`shouldSatisfy` isNothing)
      fuzzBoth @Records.Api "finds no error in a record-based generic app" Records.server R.defaultConfig (`shouldSatisfy` isNothing)
      let complexSeeds = [R.hashedDyn (1 :: Int), R.hashedDyn (2 :: Int), R.hashedDyn (3 :: Int)]
      fuzzBoth @Records.ComplexApi "finds no error in a complex record-based generic app" Records.complexServer R.defaultConfig { R.seed = complexSeeds } (`shouldSatisfy` isNothing)
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
          seededConfig =
            R.defaultConfig
              { R.seed = [(toDyn res, hash res)],
                R.maxReps = 1
              }
      expectFailing $ fuzzBoth @Seeded.Api "finds an error using information passed in" Seeded.server
        seededConfig
        (`shouldSatisfy` isNothing)

    describe "Foo" $
      fuzzBoth @Foo.Api "finds an error in a basic app" Foo.server R.defaultConfig (`shouldSatisfy` serverFailure)

    describe "Records" $
      do
        fuzzBoth @Records.Api "finds an error in a record-based generic app that throws" Records.badServer R.defaultConfig (`shouldSatisfy` serverFailure)
        let complexSeeds = [R.hashedDyn (10 :: Int), R.hashedDyn (20 :: Int), R.hashedDyn (30 :: Int)]
        fuzzBoth @Records.ComplexApi "finds an error in a complex record-based generic app that throws" Records.complexBadServer R.defaultConfig { R.seed = complexSeeds } (`shouldSatisfy` serverFailure)

    describe "QueryParams" $
      fuzzBoth @QueryParams.Api "can handle query params" QueryParams.server R.defaultConfig { R.seed = [R.hashedDyn (12::Int)] }
      (`shouldSatisfy` isNothing)

  describe "BuildFrom" $ do
    it "deduplicates identical stash values" $ do
      let value = 42 :: Int
          tr = TR.typeRep @Int
          someTr = TR.SomeTypeRep tr
          duplicates =
            R.StashValue
              (NEL.fromList [([R.Provenance someTr 0], value), ([R.Provenance someTr 1], value)])
              (IntSet.singleton (hash value))
          stash = R.Stash (DM.singleton tr duplicates)
      case R.buildFrom @Int stash of
        Nothing -> expectationFailure "expected stash entries"
        Just stashValue -> do
          NEL.length (R.getStashValue stashValue) `shouldBe` 1
          IntSet.toList (R.stashHash stashValue) `shouldBe` [hash value]
    describe "headers (and sum types)" $
      fuzzBoth @Headers.Api "should find a failure that's dependent on using header info" Headers.server R.defaultConfig
      (`shouldSatisfy` serverFailure)
    describe "product types" $
      fuzzBoth @Product.Api "should find a failure that's dependent on creating a product" Product.server
      R.defaultConfig {R.seed = [R.hashedDyn 'a', R.hashedDyn (1 :: Int)]}
      (`shouldSatisfy` serverFailure)
  describe "Server health" $ do
    it "reports when the server port is closed" $
      withClosedPort $ \(clientEnv :: ClientEnv) -> do
        RC.fuzz @Valid.Api clientEnv R.defaultConfig { R.maxReps = 1 }
          >>= (`shouldSatisfy` serverFailure)
  describe "Breakdown" $ do
    fuzzBoth @Breakdown.ProductApi "handles products"  Breakdown.productServer R.defaultConfig
      (`shouldSatisfy` serverFailure)
    fuzzBoth @Breakdown.SumApi "handles sums" Breakdown.sumServer R.defaultConfig
      (`shouldSatisfy` serverFailure)
  describe "flattening" $
    fuzzBoth @Nested.FlatApi "can handle nested apis" Nested.server R.defaultConfig {R.coverageThreshold = 0.99}
    (`shouldSatisfy` isNothing)

  describe "Minithesis prep" $ do
    it "shrinker should minimize to two calls" $ do
      let config =
            R.defaultConfig
              { R.rngSeed = 2024,
                R.maxRuntime = 0.05,
                R.maxReps = 200
              }
      report <- RS.fuzz @MinithesisExample.Api MinithesisExample.server config
      case report of
        Nothing -> expectationFailure "expected a failure report"
        Just R.Report {rsException = R.RoboservantException {..}} -> do
          failureReason `shouldBe` R.ServerCrashed
          let callCount = length (R.path fuzzState)
          callCount `shouldBe` 2

  describe "Remote fuzzing" $ do
    it "fuzzes a server via BaseUrl" $ do
      Warp.testWithApplication (pure $ serve (Proxy @Valid.Api) Valid.server) $ \port -> do
        baseUrl <- parseBaseUrl "http://127.0.0.1"
        let remoteUrl = baseUrl { baseUrlPort = port }
        RC.fuzzBaseUrl @Valid.Api remoteUrl R.defaultConfig
          >>= (`shouldSatisfy` isNothing)

    it "fuzzes a server via URL string" $ do
      Warp.testWithApplication (pure $ serve (Proxy @Valid.Api) Valid.server) $ \port -> do
        let url = "http://127.0.0.1:" <> show port
        RC.fuzzUrl @Valid.Api url R.defaultConfig
          >>= (`shouldSatisfy` either (const False) isNothing)

  describe "Trace checks" $ do
    it "flags unauthorized responses via trace checks" $ do
      let unauthorizedToken = MinithesisAuth.Token 0
          unauthorizedCheck =
            R.TraceCheck
              { R.traceCheckName = "unauthorized access",
                R.traceCheck = unauthorizedProperty
              }
          unauthorizedProperty calls =
            case find isUnauthorized calls of
              Nothing -> Nothing
              Just _ -> Just "encountered 401 response"
          isUnauthorized R.CallTrace {R.ctResult = R.TraceError err} =
            (not . R.fatalError) err && "401" `T.isInfixOf` R.errorMessage err
          isUnauthorized _ = False
          config =
            R.defaultConfig
              { R.seed = [R.hashedDyn unauthorizedToken],
                R.traceChecks = [unauthorizedCheck],
                R.maxReps = 10,
                R.maxRuntime = 0.05,
                R.rngSeed = 2025
              }
      report <- RS.fuzz @MinithesisAuth.Api MinithesisAuth.server config
      case report of
        Nothing -> expectationFailure "expected a trace check failure"
        Just R.Report {rsException = R.RoboservantException {..}} -> do
          failureReason `shouldBe` R.TraceCheckFailed "unauthorized access" "encountered 401 response"
          length (R.trace fuzzState) `shouldBe` 1

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
deriving via (R.Atom MinithesisExample.Secret) instance R.Breakdown MinithesisExample.Secret
deriving via (R.Atom MinithesisExample.Secret) instance R.BuildFrom MinithesisExample.Secret
deriving via (R.Atom MinithesisAuth.Token) instance R.Breakdown MinithesisAuth.Token
deriving via (R.Atom MinithesisAuth.Token) instance R.BuildFrom MinithesisAuth.Token

-- | `shouldFail` allows you to assert that a given `Spec` should contain at least one failing test.
--   this is often useful when testing tests.
