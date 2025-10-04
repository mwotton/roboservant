{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Direct
  ( fuzz',
    Config (..),
    -- TODO come up with something smarter than exporting all this, we should
    -- have some nice error-display functions
    RoboservantException (..),
    FuzzState (..),
    FuzzOp (..),
    FailureType (..),
    Report (..),
  )
where

import Control.Exception.Lifted
  ( Exception,
    Handler (Handler),
    SomeAsyncException,
    SomeException,
    catch,
    catches,
    handle,
    throw,
  )
import Control.Monad.State.Strict
  ( MonadIO (..),
    MonadState (get),
    StateT,
    execStateT,
    modify',
  )
import qualified Data.Dependent.Map as DM
import Data.Dynamic (Dynamic (..))
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Vinyl.Functor as V
import GHC.Generics ((:*:) (..))
import Roboservant.Types
  ( ApiOffset (..),
    Argument (..),
    InteractionError(..),
    Provenance (..),
    ReifiedApi,
    ReifiedEndpoint (..),
    Stash (..),
    StashValue (..),
    TypedF,
  )
import Roboservant.Types.Config

import Minithesis
  ( RunOptions (..),
    TestCase,
    choice,
    property,
    resolveRunOptionsWith,
    runProperty,
  )
import qualified Type.Reflection as R

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
        stash :: Stash
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
        eoArgs :: V.Rec (TypedF StashValue) as
      }

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
fuzz' reifiedApi config = handle (pure . Just . formatException) $ do
  runOpts <- configureRunOptions config
  runProperty runOpts (property (runFuzzProperty reifiedApi config))
  pure Nothing
  where
    -- something less terrible later
    formatException :: RoboservantException -> Report
    formatException r@(RoboservantException failureType exception _state) =
      Report
        (unlines [show failureType, show exception])
        r

configureRunOptions :: Config -> IO RunOptions
configureRunOptions cfg = do
  let maxExamples = max 1 (min (maxRepsInt cfg) 64)
  resolveRunOptionsWith $ \opts ->
    opts
      { runMaxExamples = maxExamples,
        runQuiet = True,
        runSeed = Just (rngSeed cfg),
        runPrinter = logInfo cfg,
        runDatabase = Nothing,
        runDatabaseKey = "roboservant"
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
            stash = addToStash (seed cfg) mempty
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
  executeWithHandlers env pathSegment eoCall selected
  runHealthCheck env

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
  StateT FuzzState IO ()
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
  StateT FuzzState IO ()
execute env fuzzOp func selected = do
  state <- get
  liftIO $ logInfo (feConfig env) (show (fuzzOp, stash state))
  let argVals = V.rmap (\(SelectedArg _ (_ :*: val)) -> val) selected
  liftIO (V.runcurry' func argVals) >>= \case
    Left interactionErr ->
      if fatalError interactionErr
        then throw interactionErr
        else pure ()
    Right dyns ->
      modify'
        ( \fs ->
            fs {stash = addToStash (NEL.toList dyns) (stash fs)}
        )

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
        pure (offset, EndpointOption reEndpointFunc args)
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
maxRepsInt Config {..}
  | maxReps <= 0 = 1
  | otherwise =
      let upper = toInteger (maxBound :: Int)
       in fromInteger (min upper maxReps)



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
