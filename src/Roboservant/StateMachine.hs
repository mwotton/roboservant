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
( prop_sequential
, prop_concurrent
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep)
import GHC.IORef (readIORef)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Servant
import Type.Reflection (SomeTypeRep)

import Roboservant.Types
import Roboservant.Hedgehog

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> [Dynamic] -> Command n m State
callEndpoint staticRoutes _seed =
  let gen :: MonadGen n => State Symbolic -> Maybe (n (Op Symbolic))
      gen State {..}
        | any null options = Nothing
        | otherwise = Just $ do
          uncurry Op <$> chooseOne options
        where
          chooseOne ::
            MonadGen n =>
            [ ( ApiOffset,
                [(TypeRep, [Var (Opaque (IORef Dynamic)) Symbolic])]
              )
            ] ->
            n
              ( ApiOffset,
                [(TypeRep, Var (Opaque (IORef Dynamic)) Symbolic)]
              )
          chooseOne opts = do
            (offset, args) <- elementOrFail opts
            (offset,) <$> mapM (\(tr, argOpts) -> (tr,) <$> elementOrFail argOpts) args
          options :: [(ApiOffset, [(TypeRep, [Var (Opaque (IORef Dynamic)) Symbolic])])]
          options =
            mapMaybe
              ( \(offset, argreps, _retType, _dynCall) -> (offset,) <$> do
                  mapM (\x -> (x,) <$> fillableWith x) argreps
              )
              staticRoutes
          fillableWith :: TypeRep -> Maybe [Var (Opaque (IORef Dynamic)) Symbolic]
          fillableWith tr = NEL.toList <$> Map.lookup tr stateRefs
      execute ::
        (MonadIO m) =>
        Op Concrete ->
        m (Opaque (IORef Dynamic))
      execute (Preload _tr v) = fmap Opaque . liftIO $ do
        newIORef . concrete $ v
      execute (Op (ApiOffset offset) args) = do
        fmap Opaque . liftIO $ do
          realArgs <- mapM (\(_tr, v) -> readIORef (opaque v)) args
          let (_offset, _staticArgs, _ret, endpoint) = staticRoutes !! offset
              -- now, magic happens: we apply some dynamic arguments to a dynamic
              -- function and hopefully somtehing useful pops out the end.
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
              Preload tr _v -> s { stateRefs =
                                  Map.insertWith (<>) tr (pure o') stateRefs }
              (Op (ApiOffset offset) _args)  ->
                s
                  { stateRefs =
                      let (_, _, tr, _) = staticRoutes !! offset
                       in Map.insertWith
                            (<>)
                            tr
                            (pure o')
                            stateRefs
                  }
          -- , Ensure (no-500 here)
        ]

prop_sequential :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
prop_sequential server seed = do
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))

  actions <-
    forAll $ do
      Gen.sequential
        (Range.linear 1 100)
        (State mempty)
        [callEndpoint reifiedApi seed]

  executeSequential (State mempty) actions

prop_concurrent :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
prop_concurrent server seed =
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api)) in
  let initialState = State mempty
   in do
        actions <-
          forAll $
            Gen.parallel
              (Range.linear 1 50)
              (Range.linear 1 10)
              initialState
              [callEndpoint reifiedApi seed]
        test $
          executeParallel initialState actions
