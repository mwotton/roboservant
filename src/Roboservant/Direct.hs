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
  ( fuzz
  )
where

import Control.Monad.Trans.Control
import Control.Monad.State.Strict
import Control.Exception.Lifted(throw,Exception,SomeException,catch)
import System.Random
import System.Timeout.Lifted
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import Hedgehog
  ( Callback (Update),
    Command (Command),
    Concrete,
    MonadGen,
    Opaque (Opaque),
    PropertyT,
    Symbolic,
    Var,
    executeSequential,
    forAll,
    opaque,
  )
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

data RoboservantException
  = RoboservantException FailureType SomeException FuzzOp FuzzState
  deriving (Show)
-- we believe in nussink, lebowski
instance Exception RoboservantException

data FailureType
  = ServerCrashed
  | CheckerFailed
  | NoPossibleMoves
  deriving (Show,Eq)

-- update :: Callback Op (Opaque (NEL.NonEmpty Dynamic)) State
-- update = Update $ \s@State {..} op o' ->
--   case op of
--     Chewable offset tr v -> case _extract offset trs chewable of
--       Nothing -> error "internal error"
--       Just (tr, _extracted, rest) ->
--         s
--           { stateRefs =
--               -- so, what's going on here... we need to be able to grab and merge
--               -- a list of (typeref, var). this means that we need to be able to pull
--               -- out typereps symbolically too, we can't just rely on the dynamic being there.
--               --                        Map.insertWith (<>) (dynTypeRep v) _  (pure o') stateRefs,
--               Map.insertWith (<>) tr (_ o') stateRefs,
--             chewable = rest
--           }
--     (Op (ApiOffset _offset) _args) ->
--       s
--         { chewable = _ o' : chewable
--           -- Map.insertWith
--           --   (<>)
--           --   tr
--           --   (pure o')
--           -- stateRefs
--         }


data Reference = Reference
  { tr :: TypeRep
  , offset :: Int
  } deriving (Show,Eq)

data FuzzOp = FuzzOp ApiOffset [Reference]
  deriving (Show,Eq)

data Config
  = Config
  { seed :: [Dynamic]
  , maxRuntime :: Int -- seconds to test for
  , rngSeed :: Int
  , currentRng :: StdGen
  }

data FuzzState = FuzzState
  { path :: [FuzzOp]
  , stash :: Map TypeRep (NEL.NonEmpty (Reference,Dynamic))
  }
  deriving (Show)

fuzz :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api))
     => Server api
     -> Config
     -> IO ()
     -> IO ()
fuzz server Config{..} checker =
  -- either we time out without finding an error, which is fine, or we find an error
  -- and throw an exception that propagates through this.
  void $ timeout (maxRuntime * 1000000) ( execStateT (forever go) FuzzState{..})

  where

    reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))

    elementOrFail :: MonadState FuzzState m
                  => [a] -> m a
    elementOrFail = undefined

    genOp :: MonadState FuzzState m
          => m FuzzOp
    genOp = do -- fs@FuzzState{..} = do
      -- choose a call to make, from the endpoints with fillable arguments.
      (offset, args) <- elementOrFail . options =<< get
      -- choose a (symbolic) argument from each. we drop the Dynamic here because it's
      -- not showable, which is no good for tests - though possibly it would make sense
      -- to use the constrained-dynamic stuff here? (or even just stash the string form)
      r <- (FuzzOp offset) <$> mapM (elementOrFail . fmap fst . NEL.toList) args
      modify' (\f -> f { path = r:path f })
      pure r


--           chooseOne opts = do
--
--             (offset,) <$> mapM elementOrFail args
      where
        options :: FuzzState -> [(ApiOffset, [NEL.NonEmpty (Reference, Dynamic)])]
        options FuzzState{..} =
          mapMaybe
            ( \(offset, (argreps, _dynCall)) -> (offset,) <$> do
                mapM (flip Map.lookup stash) argreps
            )
            reifiedApi
    execute :: MonadState FuzzState m
          => FuzzOp -> m ()
    execute = undefined



        --     execute (Op (ApiOffset offset) args) = do
        -- fmap Opaque . liftIO $ do
        --   let realArgs = map opaque args
        --   let (_offset, (_staticArgs, endpoint)) = staticRoutes !! offset
        --       -- now, magic happens: we apply some dynamic arguments to a dynamic
        --       -- function and hopefully something useful pops out the end.
        --       func = foldr (\arg curr -> flip dynApply arg =<< curr) (Just endpoint) realArgs
        --   let showable = "blah" -- map dynTypeRep (_endpoint : realArgs)
        --   case func of
        --     Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
        --     Just (f') -> do
        --       case fromDynamic f' of
        --         Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
        --         Just f -> liftIO f >>= \case
        --           Left (serverError :: ServerError) -> error (show serverError)
        --           Right (_typeRep :: SomeTypeRep, (dyn :: NEL.NonEmpty Dynamic)) -> pure dyn


    go :: (MonadState FuzzState m, MonadIO m, MonadBaseControl IO m)
         => m ()
    go = do
      fuzzOp <- genOp
      catch (execute fuzzOp)
        (\(e :: SomeException) -> throw . RoboservantException ServerCrashed e fuzzOp =<< get)
      catch (liftIO checker)
        (\(e :: SomeException) -> throw . RoboservantException CheckerFailed e fuzzOp =<< get)

  -- actions <-
  --   forAll $ do
  --     Gen.sequential
  --       (Range.linear 1 100)
  --       emptyState
  --       (fmap preload seed <> [callEndpoint reifiedApi])
      --  executeSequential emptyState actions
