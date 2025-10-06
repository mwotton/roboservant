{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Maybe (isJust, isNothing)
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
import qualified SummaryExample
import qualified Minithesis.Sydtest as MinithesisSydtest
import Test.Syd
import qualified Valid
import Servant ( Server, Proxy(..), serve, Endpoints, HasServer )

import Servant.Client(ClientEnv, mkClientEnv, baseUrlPort, parseBaseUrl,HasClient,ClientM)
import Network.Wai(Application)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Client       (Manager, defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import qualified Network.Socket as Socket
import qualified Type.Reflection as TR
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Minithesis (RunOptions (..), defaultRunOptions, resolveRunOptions, runDatabase, runDatabaseKey)
import Minithesis.Integration (databaseDisabled, hashedLabel, scopedCallsite, scopedDatabase)
import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)

newTestManager :: IO Manager
newTestManager = newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 1000000 }

main :: IO ()
main = sydTest spec

data ExpectedOutcome
  = ExpectPass (Maybe R.Report -> IO ())
  | ExpectFail (Maybe R.Report -> IO ())

fuzzBoth
  :: forall a.
     ( R.ToReifiedApi (Endpoints a)
     , HasServer a '[]
     , RS.FlattenServer a
     , RC.ToReifiedClientApi (Endpoints a)
     , RC.FlattenClient a
     , HasClient ClientM a
     )
  => ExpectedOutcome
  -> String
  -> Server a
  -> R.Config
  -> Spec
fuzzBoth expected name server config = do
  let (checkResult, wrapProperty) = case expected of
        ExpectPass cond -> (cond, id)
        ExpectFail cond -> (cond, expectFailing)

  wrapProperty $
    MinithesisSydtest.prop (name <> " via server (property)") $
      RS.fuzzProperty @a server config

  wrapProperty $
    around (withServer (serve (Proxy :: Proxy a) server)) $
      propWithEnv (name <> " via client (property)") $ \(clientEnv :: ClientEnv) ->
        RC.fuzzProperty @a clientEnv config

  it (name <> " via server") $ do
    report <- RS.fuzz @a server config
    checkResult report

  around (withServer (serve (Proxy :: Proxy a) server)) $
    it (name <> " via client") $ \(clientEnv :: ClientEnv) -> do
      RC.fuzz @a clientEnv config >>= checkResult

withServer :: Application -> ((ClientEnv -> IO ()) -> IO ())
withServer app inner = Warp.testWithApplication (pure app) $ \port -> do
  env <- genClientEnv port
  inner env
  where genClientEnv port = do
          baseUrl <- parseBaseUrl "http://localhost"
          manager <- newTestManager
          pure $ mkClientEnv manager (baseUrl { baseUrlPort = port })

propWithEnv ::
  (HasCallStack, ToProperty p) =>
  String ->
  (a -> p) ->
  SpecWith a
propWithEnv = propWithEnvWith defaultRunOptions

propWithEnvWith ::
  (HasCallStack, ToProperty p) =>
  RunOptions ->
  String ->
  (a -> p) ->
  SpecWith a
propWithEnvWith base name mkProperty =
  withFrozenCallStack $
    it name $ \env -> do
      let propertyValue = toProperty (mkProperty env)
          callsite = scopedCallsite [".propWithEnvWith", ".propWithEnv"]
          fixedKey = "sydtest-" <> hashedLabel (name <> "@" <> callsite)
      opts0 <- resolveRunOptions (applyPropertyOptions base propertyValue)
      dbDisabled <- databaseDisabled
      opts <-
        if dbDisabled
          then pure opts0 {runDatabase = Nothing}
          else do
            db <- scopedDatabase fixedKey
            pure opts0 {runDatabase = Just db, runDatabaseKey = fixedKey}
      runProperty opts propertyValue

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
      fuzzBoth @Valid.Api (ExpectPass (`shouldSatisfy` isNothing)) "find no error in a basic app" Valid.server R.defaultConfig
      fuzzBoth @Valid.RoutedApi (ExpectPass (`shouldSatisfy` isNothing)) "finds no error in a valid generic app"   Valid.routedServer R.defaultConfig
      fuzzBoth @Records.Api (ExpectPass (`shouldSatisfy` isNothing)) "finds no error in a record-based generic app" Records.server R.defaultConfig
      let complexSeeds = [R.hashedDyn (1 :: Int), R.hashedDyn (2 :: Int), R.hashedDyn (3 :: Int)]
      fuzzBoth @Records.ComplexApi (ExpectPass (`shouldSatisfy` isNothing)) "finds no error in a complex record-based generic app" Records.complexServer R.defaultConfig { R.seed = complexSeeds }
      fuzzBoth @Valid.Api
        (ExpectFail (\r ->
           fmap (R.failureReason . R.rsException) r
           `shouldSatisfy` ( \case
                               Just (R.InsufficientCoverage _) -> True
                               _ -> False
                           )))
        "fails coverage check"
        Valid.server
        R.defaultConfig {R.coverageThreshold = 0.6}
    describe "posted body" $
      fuzzBoth @Post.Api (ExpectPass (`shouldSatisfy` isNothing)) "passes a coverage check using a posted body" Post.server R.defaultConfig {R.coverageThreshold = 0.99}


    describe "PUTted body" $
      fuzzBoth @Put.Api (ExpectPass (`shouldSatisfy` isNothing)) "passes a coverage check using a posted body" Put.server R.defaultConfig {R.coverageThreshold = 0.99}


    describe "seeded" $ do
      let res = Seeded.Seed 1
          seededConfig =
            R.defaultConfig
              { R.seed = [(toDyn res, hash res)],
                R.maxReps = 1
              }
      fuzzBoth @Seeded.Api (ExpectFail (`shouldSatisfy` serverFailure)) "finds an error using information passed in" Seeded.server
        seededConfig

    describe "Foo" $
      fuzzBoth @Foo.Api (ExpectFail (`shouldSatisfy` serverFailure)) "finds an error in a basic app" Foo.server R.defaultConfig

    describe "Records" $
      do
        fuzzBoth @Records.Api (ExpectFail (`shouldSatisfy` serverFailure)) "finds an error in a record-based generic app that throws" Records.badServer R.defaultConfig
        let complexSeeds = [R.hashedDyn (10 :: Int), R.hashedDyn (20 :: Int), R.hashedDyn (30 :: Int)]
        fuzzBoth @Records.ComplexApi (ExpectFail (`shouldSatisfy` serverFailure)) "finds an error in a complex record-based generic app that throws" Records.complexBadServer R.defaultConfig { R.seed = complexSeeds }

    describe "QueryParams" $
      fuzzBoth @QueryParams.Api (ExpectPass (`shouldSatisfy` isNothing)) "can handle query params" QueryParams.server R.defaultConfig { R.seed = [R.hashedDyn (12::Int)] }

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
      fuzzBoth @Headers.Api (ExpectFail (`shouldSatisfy` serverFailure)) "should find a failure that's dependent on using header info" Headers.server R.defaultConfig
    describe "product types" $
      fuzzBoth @Product.Api (ExpectFail (`shouldSatisfy` serverFailure)) "should find a failure that's dependent on creating a product" Product.server
      R.defaultConfig {R.seed = [R.hashedDyn 'a', R.hashedDyn (1 :: Int)]}
  describe "Server health" $ do
    it "reports when the server port is closed" $
      withClosedPort $ \(clientEnv :: ClientEnv) -> do
        RC.fuzz @Valid.Api clientEnv R.defaultConfig { R.maxReps = 1 }
          >>= (`shouldSatisfy` serverFailure)
  describe "Breakdown" $ do
    fuzzBoth @Breakdown.ProductApi (ExpectFail (`shouldSatisfy` serverFailure)) "handles products"  Breakdown.productServer R.defaultConfig
    fuzzBoth @Breakdown.SumApi (ExpectFail (`shouldSatisfy` serverFailure)) "handles sums" Breakdown.sumServer R.defaultConfig
  describe "flattening" $
    fuzzBoth @Nested.FlatApi (ExpectPass (`shouldSatisfy` isNothing)) "can handle nested apis" Nested.server R.defaultConfig {R.coverageThreshold = 0.99}

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

    it "captures call summaries for reporting" $ do
      let config =
            R.defaultConfig
              { R.rngSeed = 2024,
                R.maxRuntime = 0.05,
                R.maxReps = 200
              }
      report <- RS.fuzz @MinithesisExample.Api MinithesisExample.server config
      case report of
        Nothing -> expectationFailure "expected a failure report"
        Just R.Report {rsException = R.RoboservantException {fuzzState = state}} -> do
          let traces = R.trace state
          length traces `shouldBe` 2
          let lastSummary = R.ctSummary (last traces)
          R.csMethod lastSummary `shouldBe` "GET"
          R.csPathSegments lastSummary `shouldBe` ["fail", "<Secret>"]
          case R.csOutcome lastSummary of
            R.CallFailed err -> R.errorMessage err `shouldSatisfy` ("explosion" `T.isInfixOf`)
            outcome -> expectationFailure $ "expected CallFailed, saw: " <> show outcome

  describe "Call summaries" $ do
    it "includes path, query parameters, body, and response" $ do
      let config =
            R.defaultConfig
              { R.seed =
                  [ R.hashedDyn (123 :: Int),
                    R.hashedDyn (456 :: Int)
                  ],
                R.traceChecks =
                  [ R.TraceCheck
                      { R.traceCheckName = "stop after first call",
                        R.traceCheck = \calls ->
                          if null calls then Nothing else Just "done"
                      }
                  ],
                R.maxReps = 1,
                R.maxRuntime = 0.01,
                R.rngSeed = 0
              }
      report <- RS.fuzz @SummaryExample.Api SummaryExample.server config
      case report of
        Nothing -> expectationFailure "expected a trace check failure"
        Just R.Report {rsException = R.RoboservantException {failureReason, fuzzState}} -> do
          failureReason `shouldBe` R.TraceCheckFailed "stop after first call" "done"
          case R.trace fuzzState of
            [call] -> do
              let summary = R.ctSummary call
              R.csMethod summary `shouldBe` "POST"
              R.csStatus summary `shouldBe` Just 200
              R.csPathSegments summary `shouldBe` ["summary"]
              case R.csQueryItems summary of
                [(key, value)] -> do
                  key `shouldBe` "id"
                  (readMaybe (T.unpack value) :: Maybe Int) `shouldSatisfy` isJust
                items -> expectationFailure $ "expected single query item, saw " <> show items
              case R.csBody summary of
                [bodyChunk] ->
                  (readMaybe (T.unpack bodyChunk) :: Maybe Int) `shouldSatisfy` isJust
                chunks -> expectationFailure $ "expected single body chunk, saw " <> show chunks
              case R.csOutcome summary of
                R.CallSucceeded responses -> do
                  responses `shouldNotBe` []
                  mapM_
                    (\responseText ->
                        (readMaybe (T.unpack responseText) :: Maybe Int) `shouldSatisfy` isJust
                    )
                    responses
                outcome -> expectationFailure $ "expected CallSucceeded, saw " <> show outcome
              let rendered = R.callSummaryLines summary
                  expected =
                    [ "POST /summary?id=456 -> 200 ok"
                    , "  body: 456"
                    , "  response: 912"
                    ]
              rendered `shouldBe` expected
            calls -> expectationFailure $ "expected exactly one trace entry, saw " <> show (length calls)

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
