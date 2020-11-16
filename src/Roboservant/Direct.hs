{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
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
  )
where

import Control.Monad.Trans.Control
import Control.Monad.State.Strict
import Control.Exception.Lifted(throw,Handler(..), Exception,SomeException,SomeAsyncException, catch, catches)
import System.Random
import System.Timeout.Lifted
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic, toDyn)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import Roboservant.Hedgehog (elementOrFail)
import Roboservant.Types
  ( ApiOffset (..),
    FlattenServer (..),
    Op (..),
    ReifiedApi,
    State (..),
    ToReifiedApi (..),
    emptyState,
  )
import Servant (Endpoints, Proxy (Proxy), Server, ServerError)
import Type.Reflection (SomeTypeRep(..),withTypeable)
--    prop_concurrent,
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Roboservant.Hedgehog (elementOrFail)
import Roboservant.Types
  ( ApiOffset (..),
    FlattenServer (..),
    Op (..),
    ReifiedApi,
    State (..),
    ToReifiedApi (..),
    emptyState,
  )
import Servant (Endpoints, Proxy (Proxy), Server, ServerError)
import Type.Reflection (SomeTypeRep)
import Roboservant.Types.Breakdown

data RoboservantException
  = RoboservantException FailureType (Maybe SomeException) FuzzState
  deriving (Show)
-- we believe in nussink, lebowski
instance Exception RoboservantException

data FailureType
  = ServerCrashed
  | CheckerFailed
  | NoPossibleMoves
  deriving (Show,Eq)


data FuzzOp = FuzzOp ApiOffset [Provenance]
  deriving (Show,Eq)

data Config
  = Config
  { seed :: [Dynamic]
  , maxRuntime :: Int -- seconds to test for
  , maxReps :: Int
  , rngSeed :: Int
  }

data FuzzState = FuzzState
  { path :: [FuzzOp]
  , stash :: Stash
  , currentRng :: StdGen
  }
  deriving (Show)

fuzz :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api))
     => Server api
     -> Config
     -> IO ()
     -> IO ()
fuzz server Config{..} checker = do
  let path = []
      stash = addToStash seed mempty
      currentRng = mkStdGen rngSeed
  -- either we time out without finding an error, which is fine, or we find an error
  -- and throw an exception that propagates through this.

  void $ timeout (maxRuntime * 1000000) ( execStateT (replicateM maxReps go) FuzzState{..})
  mapM_ (print . (\(offset, (args, dyn) ) -> (offset, map fst args, dyn))) reifiedApi

  where

    reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))

    elementOrFail :: (MonadState FuzzState m, MonadIO m)
                  => [a] -> m a
    elementOrFail [] = liftIO . throw . RoboservantException NoPossibleMoves Nothing =<< get
    elementOrFail l = do
      st <- get
      let (index,newGen) = randomR (0, length l - 1) (currentRng st)
      modify' $ \st -> st { currentRng = newGen }
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
            ( \(offset, (argreps, dynCall)) -> (offset,dynCall,) <$> do
                -- tricky: need to have some kind of type witness here.
                mapM (\(tr,bf) -> bf stash ) argreps
            )
            reifiedApi

    execute :: (MonadState FuzzState m, MonadIO m)
          => (FuzzOp,Dynamic,[Dynamic])  -> m ()
    execute (fuzzop, dyncall, args) = do
      liftIO $ print fuzzop
      -- now, magic happens: we apply some dynamic arguments to a dynamic
      -- function and hopefully something useful pops out the end.
      let func = foldr (\arg curr -> flip dynApply arg =<< curr) (Just dyncall) args
      st <- get
      let showable = unlines $ ("args":map (show . dynTypeRep) args)
            <> ["fuzzop"
               , show fuzzop
               ,"dyncall"
               ,show (dynTypeRep dyncall)
               ,"state"
               ,show st]
      liftIO $ putStrLn showable
      
      result <- case func of
        Nothing -> error ("all screwed up 1: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
        Just (f') -> do
          -- liftIO $ print
          case fromDynamic f' of
            Nothing -> error ("all screwed up 2: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
            Just (f) -> liftIO f >>= \case
              Left (serverError :: ServerError) -> error (show serverError)
              Right (dyn :: NEL.NonEmpty Dynamic) -> do
                liftIO $ print ("storing", fmap dynTypeRep dyn)
                pure dyn
--              Right (_typeRep :: SomeTypeRep, (dyn :: NEL.NonEmpty Dynamic)) -> pure dyn
      -- do stuff with result TODO
      modify' (\f@FuzzState{..} ->
                 f { stash = addToStash (NEL.toList result) stash } )
      pure ()

    go :: (MonadState FuzzState m, MonadIO m, MonadBaseControl IO m)
         => m ()
    go = do
      op@(fuzzOp,_,_) <- genOp
      catches (execute op)
        [ Handler (\(e :: SomeAsyncException) -> throw e)
        , Handler (\(e :: SomeException) -> throw . RoboservantException ServerCrashed (Just e)  =<< get)
        ]
      catch (liftIO checker)
        (\(e :: SomeException) -> throw . RoboservantException CheckerFailed (Just e)  =<< get)

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
