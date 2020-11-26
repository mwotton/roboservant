{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Direct
  ( fuzz, Config(..)

  -- TODO come up with something smarter than exporting all this, we should
  -- have some nice error-display functions
  , RoboservantException(..), FuzzState(..), FuzzOp(..)
  )
where

import Control.Exception.Lifted(throw,handle,Handler(..), Exception,SomeException,SomeAsyncException, catch, catches)
import Control.Monad(void,replicateM)
import Control.Monad.State.Strict(MonadState,MonadIO,get,modify',liftIO,runStateT)
import Control.Exception.Lifted(throw,Handler(..), Exception,SomeException,SomeAsyncException, catch, catches)
import Control.Monad(void,replicateM,forM_)
import Control.Monad.State.Strict(MonadState,MonadIO,get,modify',liftIO,execStateT)
import Control.Monad.Trans.Control(MonadBaseControl)
import Data.Dependent.Map (DMap)
import Data.Dynamic (Dynamic(..), dynApply, dynTypeRep, fromDynamic)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import Servant (Endpoints, Proxy (Proxy), Server, ServerError(..))
import System.Random(StdGen,randomR,mkStdGen)
import System.Timeout.Lifted(timeout)
import GHC.Generics ((:*:)(..))
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.Curry as V
import qualified Data.Dependent.Map as DM
import qualified Data.Vinyl as V
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import qualified Type.Reflection as R

import Roboservant.Types.Breakdown
import Roboservant.Types
  ( ApiOffset (..),
    FlattenServer (..),
--    ReifiedApi,
    ToReifiedApi (..),
    ReifiedEndpoint(..),
    Argument(..),
    TypedF

  )

data RoboservantException
  = RoboservantException FailureType (Maybe SomeException) Int FuzzState
  deriving (Show)
-- we believe in nussink, lebowski
instance Exception RoboservantException

data FailureType
  = ServerCrashed
  | CheckerFailed
  | NoPossibleMoves
  | InsufficientCoverage
  deriving (Show,Eq)


data FuzzOp = FuzzOp
  { apiOffset :: ApiOffset
  , provenance :: [Provenance]
  } deriving (Show,Eq)

data Config
  = Config
  { seed :: [Dynamic]
  , maxRuntime :: Integer -- seconds to test for
  , maxReps :: Integer
  , rngSeed :: Int
  , coverageThreshold :: Double
  }

data FuzzState = FuzzState
  { path :: [FuzzOp]
  , stash :: Stash
  , currentRng :: StdGen
  }
  deriving Show

data EndpointOption = forall as. (V.RecordToList as, V.RMap as) => EndpointOption
    { eoCall :: V.Curried as (IO (Either ServerError (NonEmpty Dynamic)))
    , eoArgs :: V.Rec (TypedF StashValue) as
    }

data StopReason
  = TimedOut
  | HitMaxIterations
  deriving (Show,Eq)

data Report = Report
  { textual :: String }
  deriving (Show,Eq)

fuzz :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api))
     => Server api
     -> Config
     -> IO ()
     -> IO (Maybe Report)
fuzz server Config{..} checker = handle (pure . Just . formatException) $ do
  let path = []
      stash = addToStash seed mempty
      currentRng = mkStdGen rngSeed


  deadline :: UTCTime <- addUTCTime (fromInteger $ maxRuntime * 1000000) <$> getCurrentTime
  -- either we time out without finding an error, which is fine, or we find an error
  -- and throw an exception that propagates through this.

  (stopreason, fs ) <- runStateT
    (untilDone (maxReps, deadline) go <* (evaluateCoverage =<< get)) FuzzState{..}
  pure Nothing
  -- mapM_ (print . (\(offset, (args, dyn) ) -> (offset, map fst args, dyn))) reifiedApi


  -- void $ timeout (maxRuntime * 1000000) ( execStateT (replicateM maxReps go) FuzzState{..})
  -- forM_ reifiedApi $ \(offset, ReifiedEndpoint{..}) ->
  --   print (
  --       offset
  --     , recordToList' (\(tr :*: _) -> R.SomeTypeRep tr) reArguments
  --     )
  where
    -- something less terrible later
    formatException :: RoboservantException -> Report
    formatException = Report . show

  --    evaluateCoverage :: FuzzState -> m ()
    evaluateCoverage f@FuzzState{..}
      | hitRoutes / totalRoutes > coverageThreshold = do
          liftIO $ print ("passed coverage", hitRoutes, totalRoutes)
          pure ()
      | otherwise = throw $ RoboservantException InsufficientCoverage Nothing routeCount f
      where hitRoutes = (fromIntegral . Set.size . Set.fromList $ map apiOffset path)
            totalRoutes = (fromIntegral routeCount)


    untilDone :: MonadIO m => (Integer,UTCTime) -> m a -> m StopReason
    untilDone (0,_) _ =  pure HitMaxIterations
    untilDone (n, deadline) action = do
      now <- liftIO getCurrentTime
      if now > deadline
        then pure TimedOut
        else do
          action
          untilDone (n-1, deadline) action

    reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
    routeCount = length reifiedApi

    elementOrFail :: (MonadState FuzzState m, MonadIO m)
                  => [a] -> m a
    elementOrFail [] = liftIO . throw . RoboservantException NoPossibleMoves Nothing routeCount =<< get
    elementOrFail l = do
      st <- get
      let (index,newGen) = randomR (0, length l - 1) (currentRng st)
      modify' $ \st' -> st' { currentRng = newGen }
      pure (l !! index)

    withOp :: (MonadState FuzzState m, MonadIO m)
          => (forall as. (V.RecordToList as, V.RMap as)
                  => FuzzOp
                  -> V.Curried as (IO (Either ServerError (NonEmpty Dynamic)))
                  -> V.Rec (TypedF V.Identity) as -> m r
             )
          -> m r
    withOp callback = do
      -- choose a call to make, from the endpoints with fillable arguments.
      (offset, EndpointOption{..}) <- elementOrFail . options =<< get
      r <- V.rtraverse
        (\(tr :*: StashValue svs) -> elementOrFail $
            zipWith (\i xy -> V.Const i :*: tr :*: xy)
               [0..]
               (NEL.toList svs)
        )
        eoArgs
      let pathSegment = FuzzOp offset $
            recordToList'
              (\(V.Const index :*: tr :*: _) -> Provenance (R.SomeTypeRep tr) index)
              r
          argValues = V.rmap
            (\(_ :*: tr :*: (_, x)) -> tr :*: V.Identity x)
            r
      modify' (\f -> f { path = path f <> [pathSegment] })
      callback pathSegment eoCall argValues
      where
        options :: FuzzState -> [(ApiOffset, EndpointOption)]
        options FuzzState{..} =
          mapMaybe
            ( \(offset, ReifiedEndpoint{..}) -> do
              args <- V.rtraverse (\(tr :*: Argument bf) -> (tr :*:) <$> bf stash) reArguments
              pure (offset, EndpointOption reEndpointFunc args)
            )
            reifiedApi

    execute :: (MonadState FuzzState m, MonadIO m, V.RecordToList as, V.RMap as)
          => FuzzOp
          -> V.Curried as (IO (Either ServerError (NonEmpty Dynamic)))
          -> V.Rec (TypedF V.Identity) as
          -> m ()
    execute fuzzop func args = do
      liftIO $ print fuzzop
      st <- get
      let showable = unlines $ ("args":map show argTypes)
            <> ["fuzzop"
               , show fuzzop
               -- ,"dyncall"
               -- ,show (dynTypeRep dyncall)
               ,"state"
               ,show st]
      liftIO $ putStrLn showable
      liftIO (V.runcurry' func argVals) >>= \case
        -- parameterise this
        Left (serverError :: ServerError) ->
          case errHTTPCode serverError of
            500 -> throw serverError
            _ -> do
              liftIO $ print ("ignoring non-500 error" , serverError)
        Right (dyn :: NEL.NonEmpty Dynamic) -> do
          liftIO $ print ("storing", fmap dynTypeRep dyn)
          modify' (\fs@FuzzState{..} ->
            fs { stash = addToStash (NEL.toList dyn) stash } )
      where
        argVals = V.rmap (\(_ :*: V.Identity x) -> V.Identity x) args
        argTypes = recordToList' (\(tr :*: _) -> R.SomeTypeRep tr) args

    go :: (MonadState FuzzState m, MonadIO m, MonadBaseControl IO m)
         => m ()
    go = withOp $ \op func args -> do
      catches (execute op func args)
        [ Handler (\(e :: SomeAsyncException) -> throw e)
        , Handler (\(e :: SomeException) -> throw . RoboservantException ServerCrashed (Just e) routeCount  =<< get)
        ]
      catch (liftIO checker)
        (\(e :: SomeException) -> throw . RoboservantException CheckerFailed (Just e) routeCount  =<< get)

addToStash :: [Dynamic]
           -> Stash
           -> Stash
addToStash result stash =
  foldr (\(Dynamic tr x) (Stash dict) -> Stash $
      DM.insertWith renumber tr (StashValue (([Provenance (R.SomeTypeRep tr) 0],x):|[])) dict
      -- in  DM.insertWith renumber _ (pure ([Provenance tr 0],x)) dict
    )
    stash
    result
-- Map.insertWith (flip (<>)) (dynTypeRep result) (_pure result) stash   })
  where
    renumber :: StashValue a
             -> StashValue a
             -> StashValue a
    renumber (StashValue singleDyn) (StashValue l) = StashValue $ case NEL.toList singleDyn of
      [([Provenance tr _], dyn)] -> l
        <> pure ([Provenance tr (length (NEL.last l) + 1)], dyn)
      _ -> error "should be impossible"


-- why isn't this in vinyl?
recordToList'
    :: (V.RecordToList as, V.RMap as)
    => (forall x. f x -> a)
    -> V.Rec f as
    -> [a]
recordToList' f = V.recordToList . V.rmap (V.Const . f)
