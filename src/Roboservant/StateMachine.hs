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

module Roboservant.StateMachine
  ( prop_sequential,
    prop_concurrent,
  )
where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import Debug.Trace
import GHC.IORef (readIORef)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Roboservant.Hedgehog
import Roboservant.Types
import Servant
import Type.Reflection (SomeTypeRep)

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> Command n m State
callEndpoint staticRoutes =
  let gen :: MonadGen n => State Symbolic -> Maybe (n (Op Symbolic))
      gen State {..} = case chewable of
        -- Chewable values still need to be broken down.
        (c : _) -> Just $ pure $ Chewable c
        _ -> do
          guard $ not $ null options
          pure $ uncurry Op <$> chooseOne options
        where
          chooseOne ::
            MonadGen n =>
            [ ( ApiOffset,
                [[Var (Opaque (IORef Dynamic)) Symbolic]]
              )
            ] ->
            n
              ( ApiOffset,
                [Var (Opaque (IORef Dynamic)) Symbolic]
              )
          chooseOne opts = do
            (offset, args) <- elementOrFail opts
            (offset,) <$> mapM elementOrFail args
          options :: [(ApiOffset, [[Var (Opaque (IORef Dynamic)) Symbolic]])]
          options =
            mapMaybe
              ( \(offset, (argreps, _retType, _dynCall)) -> (offset,) <$> do
                  mapM fillableWith argreps
              )
              staticRoutes
          fillableWith :: TypeRep -> Maybe [Var (Opaque (IORef Dynamic)) Symbolic]
          fillableWith tr = NEL.toList <$> Map.lookup tr stateRefs
      execute ::
        (MonadIO m) =>
        Op Concrete ->
        m (Opaque (IORef Dynamic))
      -- bit subtle here - a chewable v _also_ gets a var, because chewing it should
      -- only give subcomponents.
      execute (Chewable v) = Opaque <$> liftIO (newIORef v)
      execute (Op (ApiOffset offset) args) = do
        fmap Opaque . liftIO $ do
          realArgs <- mapM (readIORef . opaque) args
          let (_offset, (_staticArgs, _ret, endpoint)) = staticRoutes !! offset
              -- now, magic happens: we apply some dynamic arguments to a dynamic
              -- function and hopefully something useful pops out the end.
              func = foldr (\arg curr -> flip dynApply arg =<< curr) (Just endpoint) realArgs
          let showable = map dynTypeRep (endpoint : realArgs)
          case func of
            Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
            Just (f') -> do
              case fromDynamic f' of
                Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
                Just f -> liftIO f >>= \case
                  Left (serverError :: ServerError) -> error (show serverError)
                  Right (_typeRep :: SomeTypeRep, (dyn :: Dynamic)) -> newIORef dyn
   in Command
        gen
        execute
        [ Update $ \s@State {..} op o' ->
            case op of
              Chewable v ->
                let (c : cs) = chewable
                 in s
                      { stateRefs =
                          Map.insertWith (<>) (dynTypeRep v) (pure o') stateRefs,
                        chewable = (breakdownDynamic c) <> cs
                      }
              (Op (ApiOffset offset) _args) ->
                s
                  { stateRefs =
                      let (_, (_, tr, _)) = staticRoutes !! offset
                       in Map.insertWith
                            (<>)
                            tr
                            (pure o')
                            stateRefs
                  }
        ]

-- | just a stub for now - this is where we probably have to start using ConstrainedDynamic
--   so we can pack them in with a Breakdown constraint. TODO.
breakdownDynamic :: Dynamic -> [Dynamic]
breakdownDynamic = const []

prop_sequential :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
prop_sequential server seed = do
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
  actions <-
    forAll $ do
      Gen.sequential
        (Range.linear 1 100)
        (emptyState {chewable = seed})
        [callEndpoint reifiedApi]
  executeSequential (emptyState {chewable = seed}) actions

prop_concurrent :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
prop_concurrent server seed =
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
   in do
        actions <-
          forAll $
            Gen.parallel
              (Range.linear 1 50)
              (Range.linear 1 10)
              (emptyState {chewable = seed})
              [callEndpoint reifiedApi]
        test $
          executeParallel (emptyState {chewable = seed}) actions
