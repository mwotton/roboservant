{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Direct
  ( fuzz',
    fuzzProperty,
    runFuzzTestCase,
    Config (..),
    -- TODO come up with something smarter than exporting all this, we should
    -- have some nice error-display functions
    RoboservantException (..),
    FuzzState (..),
    FuzzOp (..),
    FailureType (..),
    Report (..),
    reportFromException,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Lifted
  ( Exception,
    Handler (Handler),
    SomeAsyncException,
    SomeException,
    catch,
    catches,
    handle,
    throw,
    try,
  )
import qualified Control.Exception as Exception
import Control.Applicative (asum, (<|>))
import Control.Monad (when)
import Control.Monad.State.Strict
  ( MonadIO (..),
    MonadState (get),
    StateT,
    execStateT,
    modify',
  )
import qualified Data.Dependent.Map as DM
import Data.Dynamic (Dynamic (..), dynTypeRep, fromDynamic, toDyn)
import qualified Data.IntSet as IntSet
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Functor as V
import qualified Type.Reflection as R
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import GHC.Generics ((:*:) (..))
import Roboservant.Types
  ( ApiOffset (..),
    Argument (..),
    ArgIndex (..),
    BodyPiece (..),
    EndpointDoc (..),
    HeaderPiece (..),
    InteractionError (..),
    PathPiece (..),
    Provenance (..),
    ReifiedApi,
    ReifiedEndpoint (..),
    QueryPiece (..),
    Stash (..),
    StashValue (..),
    TypedF,
  )
import Roboservant.Types.Config

import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive
  )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Minithesis
  ( Property,
    RunOptions (..),
    TestCase,
    choice,
    property,
    resolveRunOptionsWith,
    runProperty,
    scopedDatabase,
    withRunOptions,
  )

data RoboservantException
  = RoboservantException
      { failureReason :: FailureType,
        serverException :: Maybe SomeException,
        fuzzState :: FuzzState
      }
  deriving (Show)

instance Exception RoboservantException

data FailureType
  = ServerCrashed
  | CheckerFailed
  | NoPossibleMoves
  | InsufficientCoverage Double
  | TraceCheckFailed
      { traceCheckLabel :: String,
        traceCheckFailure :: String
      }
  deriving (Show, Eq)

data FuzzOp
  = FuzzOp
      { apiOffset :: ApiOffset,
        provenance :: [Provenance]
      }
  deriving (Show, Eq)

data FuzzState
  = FuzzState
      { path :: [FuzzOp],
        stash :: Stash,
        trace :: [CallTrace]
      }
  deriving (Show)

data FuzzEnv
  = FuzzEnv
      { feApi :: ReifiedApi,
        feConfig :: Config,
        feTestCase :: TestCase
      }

data EndpointOption
  = forall as.
    (V.RecordToList as, V.RMap as) =>
    EndpointOption
      { eoCall :: V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic, Int)))),
        eoArgs :: V.Rec (TypedF StashValue) as,
        eoDescribe :: V.Rec SelectedArg as -> CallSummary
      }


databaseLock :: MVar ()
databaseLock = unsafePerformIO (newMVar ())
{-# NOINLINE databaseLock #-}


selectedDynamic :: SelectedArg a -> Dynamic
selectedDynamic (SelectedArg _ (tr :*: V.Identity val)) =
  R.withTypeable tr (toDyn val)

argDynamicAt :: Int -> V.Rec SelectedArg as -> Maybe Dynamic
argDynamicAt _ V.RNil = Nothing
argDynamicAt 0 (sel V.:& _) = Just (selectedDynamic sel)
argDynamicAt n (_ V.:& rest) = argDynamicAt (n - 1) rest

boolText :: Bool -> Text
boolText True = "true"
boolText False = "false"

renderDynamicText :: Dynamic -> Text
renderDynamicText dyn =
  fromMaybe fallback (asum renderers)
  where
    fallback = "<" <> T.pack (show (dynTypeRep dyn)) <> ">"
    renderers =
      [ T.pack <$> (fromDynamic dyn :: Maybe String)
      , fromDynamic dyn :: Maybe Text
      , T.pack . show <$> (fromDynamic dyn :: Maybe Int)
      , T.pack . show <$> (fromDynamic dyn :: Maybe Integer)
      , T.pack . show <$> (fromDynamic dyn :: Maybe Double)
      , T.pack . show <$> (fromDynamic dyn :: Maybe Scientific)
      , boolText <$> (fromDynamic dyn :: Maybe Bool)
      , renderMaybeWith id (fromDynamic dyn :: Maybe (Maybe Text))
      , renderMaybeWith T.pack (fromDynamic dyn :: Maybe (Maybe String))
      , renderMaybeWith (T.pack . show) (fromDynamic dyn :: Maybe (Maybe Int))
      , renderMaybeWith boolText (fromDynamic dyn :: Maybe (Maybe Bool))
      , renderListWith id (fromDynamic dyn :: Maybe [Text])
      , renderListWith T.pack (fromDynamic dyn :: Maybe [String])
      , renderListWith (T.pack . show) (fromDynamic dyn :: Maybe [Int])
      , renderListWith (T.pack . show) (fromDynamic dyn :: Maybe [Integer])
      , renderListWith (T.pack . show) (fromDynamic dyn :: Maybe [Double])
      , renderListWith boolText (fromDynamic dyn :: Maybe [Bool])
      , renderListWith id (fmap NEL.toList (fromDynamic dyn :: Maybe (NEL.NonEmpty Text)))
      , renderListWith T.pack (fmap NEL.toList (fromDynamic dyn :: Maybe (NEL.NonEmpty String)))
      , renderListWith (T.pack . show) (fmap NEL.toList (fromDynamic dyn :: Maybe (NEL.NonEmpty Int)))
      , renderListWith boolText (fmap NEL.toList (fromDynamic dyn :: Maybe (NEL.NonEmpty Bool)))
      , decodeJSON <$> (fromDynamic dyn :: Maybe Aeson.Value)
      , decodeBytes <$> (fromDynamic dyn :: Maybe BS.ByteString)
      ]
    renderMaybeWith :: (a -> Text) -> Maybe (Maybe a) -> Maybe Text
    renderMaybeWith f mm = case mm of
      Nothing -> Nothing
      Just Nothing -> Just "Nothing"
      Just (Just x) -> Just ("Just " <> f x)
    renderListWith :: (a -> Text) -> Maybe [a] -> Maybe Text
    renderListWith f = fmap (\xs -> "[" <> T.intercalate ", " (map f xs) <> "]")
    decodeJSON value = TE.decodeUtf8 (BL.toStrict (Aeson.encode value))
    decodeBytes bs = TE.decodeUtf8 (B64.encode bs)

listValueTexts :: Dynamic -> Maybe [Text]
listValueTexts dyn =
  asum
    [ fromDynamic dyn :: Maybe [Text]
    , fmap (map T.pack) (fromDynamic dyn :: Maybe [String])
    , fmap (map (T.pack . show)) (fromDynamic dyn :: Maybe [Int])
    , fmap (map (T.pack . show)) (fromDynamic dyn :: Maybe [Integer])
    , fmap (map (T.pack . show)) (fromDynamic dyn :: Maybe [Double])
    , fmap (map boolText) (fromDynamic dyn :: Maybe [Bool])
    , fmap (map id . NEL.toList) (fromDynamic dyn :: Maybe (NEL.NonEmpty Text))
    , fmap (map T.pack . NEL.toList) (fromDynamic dyn :: Maybe (NEL.NonEmpty String))
    , fmap (map (T.pack . show) . NEL.toList) (fromDynamic dyn :: Maybe (NEL.NonEmpty Int))
    , fmap (map boolText . NEL.toList) (fromDynamic dyn :: Maybe (NEL.NonEmpty Bool))
    ]

maybeValueTexts :: Dynamic -> Maybe [Text]
maybeValueTexts dyn =
  asum
    [ toTexts (fromDynamic dyn :: Maybe (Maybe Text)) id
    , toTexts (fromDynamic dyn :: Maybe (Maybe String)) T.pack
    , toTexts (fromDynamic dyn :: Maybe (Maybe Int)) (T.pack . show)
    , toTexts (fromDynamic dyn :: Maybe (Maybe Integer)) (T.pack . show)
    , toTexts (fromDynamic dyn :: Maybe (Maybe Double)) (T.pack . show)
    , toTexts (fromDynamic dyn :: Maybe (Maybe Bool)) boolText
    ]
  where
    toTexts :: Maybe (Maybe a) -> (a -> Text) -> Maybe [Text]
    toTexts mm f = case mm of
      Nothing -> Nothing
      Just Nothing -> Just []
      Just (Just x) -> Just [f x]

maybeBoolValue :: Dynamic -> Maybe Bool
maybeBoolValue dyn =
  (fromDynamic dyn :: Maybe Bool)
    <|> do
      mb <- fromDynamic dyn :: Maybe (Maybe Bool)
      pure (fromMaybe False mb)

valueTextsForQueryParam :: Dynamic -> [Text]
valueTextsForQueryParam dyn =
  fromMaybe [renderDynamicText dyn] (maybeValueTexts dyn)

valueTextsForQueryParams :: Dynamic -> [Text]
valueTextsForQueryParams dyn =
  fromMaybe [renderDynamicText dyn] (listValueTexts dyn)

describeEndpointDoc :: EndpointDoc as -> V.Rec SelectedArg as -> CallSummary
describeEndpointDoc EndpointDoc {..} selected =
  let initial = emptySummary docMethod docStatus
      withPath = F.foldl' (applyPath selected) initial docPathPieces
      withQuery = F.foldl' (applyQuery selected) withPath docQueryPieces
      withHeaders = F.foldl' (applyHeader selected) withQuery docHeaderPieces
   in maybe withHeaders (applyBody selected withHeaders) docBodyPiece
  where
    applyPath rec summary piece = case piece of
      StaticPiece seg -> appendPathSegment seg summary
      CapturePiece label (ArgIndex idx) ->
        case argDynamicAt idx rec of
          Just dyn ->
            let value = renderDynamicText dyn
                summary' = appendPathSegment value summary
             in maybe summary' (\lbl -> appendNote (lbl <> "=" <> value) summary') label
          Nothing -> appendPathSegment "<missing>" summary

    applyQuery rec summary piece = case piece of
      QueryParamPiece name (ArgIndex idx) ->
        case argDynamicAt idx rec of
          Just dyn -> F.foldl' (\acc v -> appendQueryItem name v acc) summary (valueTextsForQueryParam dyn)
          Nothing -> summary
      QueryParamsPiece name (ArgIndex idx) ->
        case argDynamicAt idx rec of
          Just dyn -> F.foldl' (\acc v -> appendQueryItem name v acc) summary (valueTextsForQueryParams dyn)
          Nothing -> summary
      QueryFlagPiece name (ArgIndex idx) ->
        case argDynamicAt idx rec >>= maybeBoolValue of
          Just True -> appendQueryItem name "true" summary
          Just False -> summary
          Nothing -> summary

    applyHeader rec summary (HeaderPiece name (ArgIndex idx)) =
      case argDynamicAt idx rec of
        Just dyn -> appendHeaderItem name (renderDynamicText dyn) summary
        Nothing -> summary

    applyBody rec summary (BodyPiece (ArgIndex idx)) =
      case argDynamicAt idx rec of
        Just dyn -> appendBodyChunk (renderDynamicText dyn) summary
        Nothing -> summary

applyOutcome :: CallSummary -> Either InteractionError (NonEmpty (Dynamic, Int)) -> CallSummary
applyOutcome summary result =
  case result of
    Left err -> setOutcome (CallFailed err) summary
    Right dyns ->
      let values = map (renderDynamicText . fst) (NEL.toList dyns)
       in setOutcome (CallSucceeded values) summary

data StopReason
  = TimedOut
  | HitMaxIterations
  deriving (Show, Eq)

data Report
  = Report
      { textual :: String,
        rsException :: RoboservantException
      }
  deriving (Show)



-- fuzzClient :: Client api -> Config -> IO (Maybe Report)
-- fuzzClient = undefined



fuzz' ::
  ReifiedApi ->
  Config ->
  IO (Maybe Report)
fuzz' reifiedApi config = handle (pure . Just . reportFromException) $ do
  withMVar databaseLock $ \_ -> do
    runOpts <- configureRunOptions config
    runProperty runOpts (fuzzProperty reifiedApi config)
  pure Nothing

fuzzProperty ::
  ReifiedApi ->
  Config ->
  Property
fuzzProperty reifiedApi config =
  withRunOptions (applyConfigOptions config) (property (runFuzzProperty reifiedApi config))

runFuzzTestCase ::
  ReifiedApi ->
  Config ->
  TestCase ->
  IO (Maybe Report)
runFuzzTestCase reifiedApi config testCase = do
  result <- try (runFuzzProperty reifiedApi config testCase)
  case result of
    Left err -> pure (Just (reportFromException err))
    Right () -> pure Nothing

reportFromException :: RoboservantException -> Report
reportFromException r@(RoboservantException failureType exception state) =
  let headerLines =
        ["failure: " <> show failureType]
          ++ maybe [] (\e -> ["exception: " <> show e]) exception
      traceLines =
        concatMap (map T.unpack . callSummaryLines . ctSummary) (trace state)
      allLines =
        headerLines
          ++ (if null traceLines then [] else "" : traceLines)
   in Report (unlines allLines) r

configureRunOptions :: Config -> IO RunOptions
configureRunOptions cfg = do
  let maxExamples = max 1 (min (maxRepsInt cfg) 64)
      relativeDb = ".minithesis-db"
  baseOverride <- lookupEnv "MINITHESIS_DB"
  let dbPath = maybe relativeDb id baseOverride
  existsAsDir <- doesDirectoryExist dbPath
  when existsAsDir $ removeDirectoryRecursive dbPath
  let parentDir = takeDirectory dbPath
  when (parentDir /= dbPath) $ createDirectoryIfMissing True parentDir
  db <- scopedDatabase dbPath
  resolveRunOptionsWith $ \opts ->
    opts
      { runMaxExamples = maxExamples,
        runQuiet = True,
        runSeed = Just (rngSeed cfg),
        runPrinter = logInfo cfg,
        runDatabase = Just db,
        runDatabaseKey = "roboservant"
      }

applyConfigOptions :: Config -> RunOptions -> RunOptions
applyConfigOptions cfg opts =
  opts
    { runMaxExamples = max 1 (min (maxRepsInt cfg) 64),
      runQuiet = True,
      runSeed = Just (rngSeed cfg),
      runPrinter = logInfo cfg
    }

runFuzzProperty :: ReifiedApi -> Config -> TestCase -> IO ()
runFuzzProperty reifiedApi cfg testCase = do
  let env =
        FuzzEnv
          { feApi = reifiedApi,
            feConfig = cfg,
            feTestCase = testCase
          }
      initialState =
        FuzzState
          { path = [],
            stash = addToStash (seed cfg) mempty,
            trace = []
          }
  finalState <- execStateT (driveFuzzer env) initialState
  evaluateCoverage env finalState

driveFuzzer :: FuzzEnv -> StateT FuzzState IO ()
driveFuzzer env = do
  let cfg = feConfig env
      limit = maxRepsInt cfg
      totalRoutes = max 1 (length (feApi env))
      minBudget = max 1 (min limit (ceiling (coverageThreshold cfg * fromIntegral totalRoutes)))
      extraCapacity = max 0 (limit - minBudget)
      boundedExtra = min extraCapacity 64
  budgetWord <- liftIO $ choice (feTestCase env) (fromIntegral boundedExtra)
  let budget = min limit (minBudget + fromIntegral budgetWord)
  go cfg 0 budget
  where
    go cfg n budget
      | n >= budget = pure ()
      | otherwise = do
          st <- get
          let enforce = coverageThreshold cfg > 0 && coverageRatio env st < coverageThreshold cfg
          mCall <- chooseEndpoint env st enforce
          case mCall of
            Nothing -> pure ()
            Just (offset, option) -> do
              performCall env offset option
              go cfg (n + 1) budget

data SelectedArg a = SelectedArg Int (TypedF V.Identity a)

performCall ::
  FuzzEnv ->
  ApiOffset ->
  EndpointOption ->
  StateT FuzzState IO ()
performCall env offset EndpointOption {..} = do
  selected <- V.rtraverse (selectArgument env) eoArgs
  let pathSegment = mkPathSegment offset selected
  modify' $ \fs -> fs {path = path fs <> [pathSegment]}
  outcome <- try (executeWithHandlers env pathSegment eoCall selected)
  case outcome of
    Right result -> do
      let summary = applyOutcome (eoDescribe selected) result
      recordTrace pathSegment selected result summary
      runTraceChecks env
      runHealthCheck env
    Left ex@(RoboservantException {failureReason = ServerCrashed, serverException = Just some}) ->
      case Exception.fromException some of
        Just interactionErr -> do
          let summary = applyOutcome (eoDescribe selected) (Left interactionErr)
          recordTrace pathSegment selected (Left interactionErr) summary
          updatedState <- get
          throw ex {fuzzState = updatedState}
        Nothing -> throw ex
    Left ex -> throw ex

recordTrace ::
  (V.RecordToList as, V.RMap as) =>
  FuzzOp ->
  V.Rec SelectedArg as ->
  Either InteractionError (NonEmpty (Dynamic, Int)) ->
  CallSummary ->
  StateT FuzzState IO ()
recordTrace fuzzOp selected result summary =
  modify'
    (\fs ->
        fs
          { trace =
              trace fs
                <> [ CallTrace
                       { ctOffset = apiOffset fuzzOp,
                         ctProvenance = provenance fuzzOp,
                         ctArguments = selectedArguments selected,
                         ctResult = toTraceResult result,
                         ctSummary = summary
                       }
                   ]
          }
    )
  where
    selectedArguments :: (V.RecordToList as, V.RMap as) => V.Rec SelectedArg as -> [Dynamic]
    selectedArguments = recordToList' (\sel -> selectedDynamic sel)

    toTraceResult :: Either InteractionError (NonEmpty (Dynamic, Int)) -> TraceResult
    toTraceResult = \case
      Left err -> TraceError err
      Right dyns -> TraceSuccess (fmap fst dyns)

runTraceChecks :: FuzzEnv -> StateT FuzzState IO ()
runTraceChecks env = do
  state <- get
  let checks = traceChecks (feConfig env)
      failure = listToMaybe . mapMaybe (evaluateCheck (trace state)) $ checks
  case failure of
    Nothing -> pure ()
    Just (name, reason) -> throw $ RoboservantException (TraceCheckFailed name reason) Nothing state
  where
    evaluateCheck :: [CallTrace] -> TraceCheck -> Maybe (String, String)
    evaluateCheck callTrace TraceCheck {..} = (traceCheckName,) <$> traceCheck callTrace

chooseEndpoint ::
  FuzzEnv ->
  FuzzState ->
  Bool ->
  StateT FuzzState IO (Maybe (ApiOffset, EndpointOption))
chooseEndpoint env state enforceCoverage = do
  let options = availableOptions env state
      pickExisting = do
        let bound = fromIntegral (length options) - 1
        idxWord <- liftIO $ choice (feTestCase env) bound
        let idx = fromIntegral idxWord
        pure (options !! idx)
  case options of
    [] -> pure Nothing
    _ | enforceCoverage -> do
          let visited = Set.fromList (map apiOffset (path state))
              unseen = filter (\(offset, _) -> offset `Set.notMember` visited) options
          case unseen of
            (nextOffset, nextOption) : _ -> pure (Just (nextOffset, nextOption))
            [] -> Just <$> pickExisting
      | otherwise -> Just <$> pickExisting

selectArgument ::
  FuzzEnv ->
  TypedF StashValue a ->
  StateT FuzzState IO (SelectedArg a)
selectArgument env (tr :*: StashValue svs _) =
  let options =
        zipWith
          (\i (_, val) -> SelectedArg i (tr :*: V.Identity val))
          [0 ..]
          (NEL.toList svs)
   in selectFrom env options

mkPathSegment ::
  (V.RecordToList as, V.RMap as) =>
  ApiOffset ->
  V.Rec SelectedArg as ->
  FuzzOp
mkPathSegment offset rec =
  FuzzOp offset $
    recordToList'
      (\(SelectedArg idx (tr :*: _)) -> Provenance (R.SomeTypeRep tr) idx)
      rec

executeWithHandlers ::
  (V.RecordToList as, V.RMap as) =>
  FuzzEnv ->
  FuzzOp ->
  V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic, Int)))) ->
  V.Rec SelectedArg as ->
  StateT FuzzState IO (Either InteractionError (NonEmpty (Dynamic, Int)))
executeWithHandlers env fuzzOp call selected =
  catches
    (execute env fuzzOp call selected)
    [ Handler (\(e :: SomeAsyncException) -> throw e),
      Handler $ \(e :: SomeException) -> throw . RoboservantException ServerCrashed (Just e) =<< get
    ]

execute ::
  (V.RecordToList as, V.RMap as) =>
  FuzzEnv ->
  FuzzOp ->
  V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic, Int)))) ->
  V.Rec SelectedArg as ->
  StateT FuzzState IO (Either InteractionError (NonEmpty (Dynamic, Int)))
execute env fuzzOp func selected = do
  state <- get
  liftIO $ logInfo (feConfig env) (show (fuzzOp, stash state))
  let argVals = V.rmap (\(SelectedArg _ (_ :*: val)) -> val) selected
  liftIO (V.runcurry' func argVals) >>= \case
    Left interactionErr ->
      if fatalError interactionErr
        then throw interactionErr
        else pure (Left interactionErr)
    Right dyns -> do
      modify'
        ( \fs ->
            fs {stash = addToStash (NEL.toList dyns) (stash fs)}
        )
      pure (Right dyns)

runHealthCheck :: FuzzEnv -> StateT FuzzState IO ()
runHealthCheck env =
  catch
    (liftIO (healthCheck (feConfig env)))
    (\(e :: SomeException) -> throw . RoboservantException CheckerFailed (Just e) =<< get)

selectFrom :: FuzzEnv -> [a] -> StateT FuzzState IO a
selectFrom _ [] = liftIO . throw . RoboservantException NoPossibleMoves Nothing =<< get
selectFrom env xs = do
  let bound = fromIntegral (length xs - 1)
  idxWord <- liftIO $ choice (feTestCase env) bound
  let idx = fromIntegral idxWord
  pure (xs !! idx)

availableOptions :: FuzzEnv -> FuzzState -> [(ApiOffset, EndpointOption)]
availableOptions FuzzEnv {..} FuzzState {..} =
  mapMaybe
    ( \(offset, ReifiedEndpoint {..}) -> do
        args <- V.rtraverse (\(tr :*: Argument bf) -> (tr :*:) <$> bf stash) reArguments
        let describe = describeEndpointDoc reDoc
        pure (offset, EndpointOption reEndpointFunc args describe)
    )
    feApi

evaluateCoverage :: FuzzEnv -> FuzzState -> IO ()
evaluateCoverage env fState
  | coverageRatio env fState >= coverageThreshold (feConfig env) = pure ()
  | otherwise = do
      displayDiagnostics env fState
      throw $ RoboservantException (InsufficientCoverage (coverageRatio env fState)) Nothing fState

coverageRatio :: FuzzEnv -> FuzzState -> Double
coverageRatio FuzzEnv {..} FuzzState {..}
  | totalRoutes == 0 = 1
  | otherwise = fromIntegral hitRoutes / fromIntegral totalRoutes
  where
    totalRoutes = length feApi
    hitRoutes = Set.size . Set.fromList $ map apiOffset path

displayDiagnostics :: FuzzEnv -> FuzzState -> IO ()
displayDiagnostics env FuzzState {..} =
  logInfo (feConfig env) . unlines $
    ["api endpoints covered"]
      <> (map show . Set.toList . Set.fromList $ map apiOffset path)
      <> ["", "types in stash"]
      <> DM.foldrWithKey (\_ v r -> (show . NEL.length . getStashValue $ v) : r) [] (getStash stash)

maxRepsInt :: Config -> Int
maxRepsInt Config {maxReps = reps}
  | reps <= 0 = 1
  | otherwise =
      let upper = toInteger (maxBound :: Int)
       in fromInteger (min upper reps)



addToStash ::
  [(Dynamic, Int)] ->
  Stash ->
  Stash
addToStash result stash =
  foldr
    ( \(Dynamic tr x, hashed) (Stash dict) ->
        Stash $
          DM.insertWith
            renumber
            tr
            (StashValue (([Provenance (R.SomeTypeRep tr) 0], x) :| []) (IntSet.singleton hashed))
            dict
    )
    stash
    result
  where
    renumber ::
      StashValue a ->
      StashValue a ->
      StashValue a
    renumber (StashValue singleDyn singleHash) orig@(StashValue l intSet)
      | not $ IntSet.null (singleHash `IntSet.intersection` intSet) = orig
      | otherwise =
        StashValue
          ( case NEL.toList singleDyn of
              [([Provenance tr _], dyn)] ->
                l <> pure ([Provenance tr (length (NEL.last l) + 1)], dyn)
              _ -> error "should be impossible"
          )
          (IntSet.union singleHash intSet)

-- why isn't this in vinyl?
recordToList' ::
  (V.RecordToList as, V.RMap as) =>
  (forall x. f x -> a) ->
  V.Rec f as ->
  [a]
recordToList' f = V.recordToList . V.rmap (V.Const . f)
