{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Direct
  ( fuzz, Config(..)

  -- TODO come up with something smarter than exporting all this, we should
  -- have some nice error-display functions 
  , RoboservantException(..), FuzzState(..), FuzzOp(..), FailureType(..), Report(..)
  )
where

import Control.Exception.Lifted(throw,handle,Handler(..), Exception,SomeException,SomeAsyncException, catch, catches)
import Control.Monad.State.Strict(MonadState,MonadIO,get,modify',liftIO,runStateT)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import Servant (Endpoints, Proxy (Proxy), Server, ServerError(..))
import System.Random(StdGen,randomR,mkStdGen)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.List(sortOn)
import Control.Arrow(second)

import Roboservant.Types.Breakdown
import Roboservant.Types
  ( ApiOffset (..),
    FlattenServer (..),
--    ReifiedApi,
    ToReifiedApi (..),
  ))
data RoboservantException
  = RoboservantException
  { failureReason :: FailureType
  , serverException :: Maybe SomeException
  , fuzzState :: FuzzState
  } deriving (Show)

instance Exception RoboservantException

data FailureType
  = ServerCrashed
  | CheckerFailed
  | NoPossibleMoves
  | InsufficientCoverage Double
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
  deriving (Show)

data StopReason
  = TimedOut
  | HitMaxIterations
  deriving (Show,Eq)

data Report = Report
  { textual :: String
  , rsException :: RoboservantException}
  deriving (Show)

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
  (stopreason, fs ) <- runStateT
    (untilDone (maxReps, deadline) go <* (evaluateCoverage =<< get)) FuzzState{..}
  pure Nothing
  -- mapM_ (print . (\(offset, (args, dyn) ) -> (offset, map fst args, dyn))) reifiedApi

  where
    -- something less terrible later
    formatException :: RoboservantException -> Report
    formatException r@(RoboservantException failureType exception _state)  = Report
      (unlines  [ show failureType , show exception ])
      r

    displayDiagnostics FuzzState{..} = liftIO $ do
      putStrLn "api endpoints covered"
      mapM_ print (Set.toList $ Set.fromList $ map apiOffset path)
      putStrLn ""
      putStrLn "types in stash"
      mapM_
        (print . second NEL.length)
        (sortOn (show . fst) . Map.toList stash)

  --    evaluateCoverage :: FuzzState -> m ()
    evaluateCoverage f@FuzzState{..}
      | coverage > coverageThreshold = pure ()
      | otherwise = do
          displayDiagnostics f
          throw $ RoboservantException (InsufficientCoverage coverage) Nothing f
      where hitRoutes = fromIntegral . Set.size . Set.fromList $ map apiOffset path
            totalRoutes = fromIntegral routeCount
            coverage = hitRoutes / totalRoutes


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
    elementOrFail [] = liftIO . throw . RoboservantException NoPossibleMoves Nothing  =<< get
    elementOrFail l = do
      st <- get
      let (index,newGen) = randomR (0, length l - 1) (currentRng st)
      modify' $ \st' -> st' { currentRng = newGen }
      pure (l !! index)

    genOp :: (MonadState FuzzState m, MonadIO m)
          => m (FuzzOp, Dynamic, [Dynamic])
    genOp = do -- fs@FuzzState{..} = do
      -- choose a call to make, from the endpoints with fillable arguments.
      (offset, dynCall, args) <- elementOrFail . options =<< get
      r <- mapM (elementOrFail . zip [0..] . NEL.toList) args
      let pathSegment = FuzzOp offset (map (\(index,(_,dyn) ) -> Provenance (dynTypeRep dyn) index) r)
      modify' (\f -> f { path = path f <> [pathSegment] })
      pure (pathSegment, dynCall, fmap (snd . snd) r)

      where
        options :: FuzzState -> [(ApiOffset, Dynamic, [NEL.NonEmpty ([Provenance], Dynamic)])]
        options FuzzState{..} =
          mapMaybe
            ( \(offset, (argreps, dynCall)) -> (offset,dynCall,) <$>
                mapM (\(_tr,bf) -> bf stash ) argreps
            )
            reifiedApi

    execute :: (MonadState FuzzState m, MonadIO m)
          => (FuzzOp,Dynamic,[Dynamic])  -> m ()
    execute (fuzzop, dyncall, args) = do
      -- liftIO $ print fuzzop
      -- now, magic happens: we apply some dynamic arguments to a dynamic
      -- function and hopefully something useful pops out the end.
      let func = foldr (\arg curr -> flip dynApply arg =<< curr) (Just dyncall) (reverse args)
      st <- get
      let showable = unlines $ ("args":map (show . dynTypeRep) args)
            <> ["fuzzop"
               , show fuzzop
               ,"dyncall"
               ,show (dynTypeRep dyncall)
               ,"state"
               ,show st]
      -- liftIO $ putStrLn showable

      case func of
        Nothing -> error ("all screwed up 1: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
        Just f' -> do
          -- liftIO $ print
          case fromDynamic f' of
            Nothing -> error ("all screwed up 2: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
            Just f -> liftIO f >>= \case
              -- parameterise this
              Left (serverError :: ServerError) ->
                case errHTTPCode serverError of
                  500 -> throw serverError
                  _ ->
                    liftIO $ print ("ignoring non-500 error" , serverError)

              Right (dyn :: NEL.NonEmpty Dynamic) -> do
                -- liftIO $ print ("storing", fmap dynTypeRep dyn)
                modify' (\fs@FuzzState{..} ->
                  fs { stash = addToStash (NEL.toList dyn) stash } )
      pure ()

--    go :: (MonadState FuzzState m, MonadIO m, MonadBaseControl IO m)
--         => m ()
    go = do
      op <- genOp
      catches (execute op)
        [ Handler (\(e :: SomeAsyncException) -> throw e)
        , Handler (\(e :: SomeException) -> do
                      displayDiagnostics =<< get
                      throw . RoboservantException ServerCrashed (Just e)  =<< get)
        ]
      catch (liftIO checker)
        (\(e :: SomeException) -> throw . RoboservantException CheckerFailed (Just e)   =<< get)

  -- actions <-
  --   forAll $ do
  --     Gen.sequential
  --       (Range.linear 1 100)
  --       emptyState
  --       (fmap preload seed <> [callEndpoint reifiedApi])
      --  executeSequential emptyState actions
addToStash :: [Dynamic]
           -> Map TypeRep (NEL.NonEmpty ([Provenance], Dynamic))
           -> Map TypeRep (NEL.NonEmpty ([Provenance], Dynamic))
addToStash result stash =
  foldr (\dyn dict -> let tr = dynTypeRep dyn in
                        Map.insertWith renumber tr (pure ([Provenance tr 0],dyn)) dict) stash result
-- Map.insertWith (flip (<>)) (dynTypeRep result) (_pure result) stash   })
  where
    renumber :: NEL.NonEmpty ([Provenance],Dynamic)
             -> NEL.NonEmpty ([Provenance],Dynamic)
             -> NEL.NonEmpty ([Provenance],Dynamic)
    renumber singleDyn l = case NEL.toList singleDyn of
      [([Provenance tr _], dyn)] -> l
        <> pure ([Provenance tr (length (NEL.last l) + 1)], dyn)
      _ -> error "should be impossible"
