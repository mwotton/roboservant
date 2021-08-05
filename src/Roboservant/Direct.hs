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
    StateT (runStateT),
    modify',
  )
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Dependent.Map as DM
import Data.Dynamic (Dynamic (..))
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
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

import System.Random (Random (randomR), StdGen, mkStdGen)
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
        stash :: Stash,
        currentRng :: StdGen
      }
  deriving (Show)

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
fuzz' reifiedApi Config {..} = handle (pure . Just . formatException) $ do
  let path = []
      stash = addToStash seed mempty
      currentRng = mkStdGen rngSeed
  deadline :: UTCTime <- addUTCTime (realToFrac $ maxRuntime * 1000000) <$> getCurrentTime
  (stopreason, _fs) <-
    runStateT
      (untilDone (maxReps, deadline) go <* (evaluateCoverage =<< get))
      FuzzState {..}
  logInfo $ show stopreason
  pure Nothing
  where
    -- something less terrible later
    formatException :: RoboservantException -> Report
    formatException r@(RoboservantException failureType exception _state) =
      Report
        (unlines [show failureType, show exception])
        r
    displayDiagnostics FuzzState {..} = liftIO $
      logInfo $ unlines $
      ["api endpoints covered"]
        <> (map show . Set.toList . Set.fromList $ map apiOffset path)
        <> ["", "types in stash"]
        <> DM.foldrWithKey (\_ v r -> (show . NEL.length . getStashValue $ v) : r) [] (getStash stash)
    --        <> (map (show . NEL.length . getStashValue ) $ DM.assocs (getStash stash))
    --        $ \_k v ->
    --               (show . NEL.length $ getStashValue v))

    evaluateCoverage f@FuzzState {..}
      | coverage > coverageThreshold = pure ()
      | otherwise = do
        displayDiagnostics f
        throw $ RoboservantException (InsufficientCoverage coverage) Nothing f
      where
        hitRoutes = fromIntegral . Set.size . Set.fromList $ map apiOffset path
        totalRoutes = fromIntegral routeCount
        coverage = hitRoutes / totalRoutes
    untilDone :: MonadIO m => (Integer, UTCTime) -> m a -> m StopReason
    untilDone (0, _) _ = pure HitMaxIterations
    untilDone (n, deadline) action = do
      now <- liftIO getCurrentTime
      if now > deadline
        then pure TimedOut
        else do
          _ <- action
          untilDone (n -1, deadline) action

    routeCount = length reifiedApi
    elementOrFail ::
      (MonadState FuzzState m, MonadIO m) =>
      [a] ->
      m a
    elementOrFail [] = liftIO . throw . RoboservantException NoPossibleMoves Nothing =<< get
    elementOrFail l = do
      st <- get
      let (index, newGen) = randomR (0, length l - 1) (currentRng st)
      modify' $ \st' -> st' {currentRng = newGen}
      pure (l !! index)
    withOp ::
      (MonadState FuzzState m, MonadIO m) =>
      ( forall as.
        (V.RecordToList as, V.RMap as) =>
        FuzzOp ->
        V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic, Int)))) ->
        V.Rec (TypedF V.Identity) as ->
        m r
      ) ->
      m r
    withOp callback = do
      -- choose a call to make, from the endpoints with fillable arguments.
      (offset, EndpointOption {..}) <- elementOrFail . options =<< get
      r <-
        V.rtraverse
          ( \(tr :*: StashValue svs _) ->
              elementOrFail $
                zipWith
                  (\i xy -> V.Const i :*: tr :*: xy)
                  [0 ..]
                  (NEL.toList svs)
          )
          eoArgs
      let pathSegment =
            FuzzOp offset $
              recordToList'
                (\(V.Const index :*: tr :*: _) -> Provenance (R.SomeTypeRep tr) index)
                r
          argValues =
            V.rmap
              (\(_ :*: tr :*: (_, x)) -> tr :*: V.Identity x)
              r
      modify' (\f -> f {path = path f <> [pathSegment]})
      callback pathSegment eoCall argValues
      where
        options :: FuzzState -> [(ApiOffset, EndpointOption)]
        options FuzzState {..} =
          mapMaybe
            ( \(offset, ReifiedEndpoint {..}) -> do
                args <- V.rtraverse (\(tr :*: Argument bf) -> (tr :*:) <$> bf stash) reArguments
                pure (offset, EndpointOption reEndpointFunc args)
            )
            reifiedApi
    execute ::
      (MonadState FuzzState m, MonadIO m, V.RecordToList as, V.RMap as) =>
      FuzzOp ->
      V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic, Int)))) ->
      V.Rec (TypedF V.Identity) as ->
      m ()
    execute fuzzop func args = do
      (liftIO . logInfo . show . (fuzzop,) . stash) =<< get
      liftIO (V.runcurry' func argVals) >>= \case
        Left (e::InteractionError) ->
          if fatalError e
          then throw e
          else pure ()
        Right (dyn :: NEL.NonEmpty (Dynamic, Int)) ->
          modify'
          ( \fs@FuzzState {..} ->
              fs {stash = addToStash (NEL.toList dyn) stash}
          )
      where
        argVals = V.rmap (\(_ :*: V.Identity x) -> V.Identity x) args
    -- argTypes = recordToList' (\(tr :*: _) -> R.SomeTypeRep tr) args
    go ::
      (MonadState FuzzState m, MonadIO m, MonadBaseControl IO m) =>
      m ()
    go = withOp $ \op func args -> do
      catches
        (execute op func args)
        [ Handler (\(e :: SomeAsyncException) -> throw e),
          Handler
            ( \(e :: SomeException) -> 
                throw . RoboservantException ServerCrashed (Just e) =<< get
            )
        ]
      catch
        (liftIO healthCheck)
        (\(e :: SomeException) -> throw . RoboservantException CheckerFailed (Just e) =<< get)

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
