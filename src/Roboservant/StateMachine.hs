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
  )
where

--    prop_concurrent,

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
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

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> Command n m State
callEndpoint staticRoutes =
  let gen :: MonadGen n => State Symbolic -> Maybe (n (Op Symbolic))
      gen State {..} = case chewable of
        (_ : _) -> Just $ do
          (offset, v) <- Gen.element (zip [0 ..] chewable)
          pure $ Chewable offset _ _
        _ -> do
          guard $ not $ null options
          pure $ uncurry Op <$> chooseOne options
        where
          chooseOne ::
            MonadGen n =>
            [ ( a,
                [[b]]
              )
            ] ->
            n
              ( a,
                [b]
              )
          chooseOne opts = do
            (offset, args) <- elementOrFail opts
            (offset,) <$> mapM elementOrFail args
          options :: [(ApiOffset, [[Var (Opaque Dynamic) Symbolic]])]
          options =
            mapMaybe
              ( \(offset, (argreps, _dynCall)) -> (offset,) <$> do
                  mapM fillableWith argreps
              )
              staticRoutes
          fillableWith :: TypeRep -> Maybe [Var (Opaque Dynamic) Symbolic]
          fillableWith tr = NEL.toList <$> Map.lookup tr stateRefs
      execute ::
        (MonadIO m) =>
        Op Concrete ->
        m (Opaque (NEL.NonEmpty Dynamic))
      -- bit subtle here - a chewable v _also_ gets a var, because chewing it should
      -- only give subcomponents.
      execute (Chewable offset trs v) = _ -- pure . Opaque $ opaque $ pure v -- pure $ Opaque v --  <$> liftIO (newIORef v)
      execute (Op (ApiOffset offset) args) = do
        fmap Opaque . liftIO $ do
          let realArgs = map opaque args
          let (_offset, (_staticArgs, endpoint)) = staticRoutes !! offset
              -- now, magic happens: we apply some dynamic arguments to a dynamic
              -- function and hopefully something useful pops out the end.
              func = foldr (\arg curr -> flip dynApply arg =<< curr) (Just endpoint) realArgs
          let showable = "blah" -- map dynTypeRep (_endpoint : realArgs)
          case func of
            Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
            Just (f') -> do
              case fromDynamic f' of
                Nothing -> error ("all screwed up: " <> maybe ("nothing: " <> show showable) (show . dynTypeRep) func)
                Just f -> liftIO f >>= \case
                  Left (serverError :: ServerError) -> error (show serverError)
                  Right (_typeRep :: SomeTypeRep, (dyn :: NEL.NonEmpty Dynamic)) -> pure dyn
   in Command
        gen
        execute
        [ update
        ]

update :: Callback Op (Opaque (NEL.NonEmpty Dynamic)) State
update = Update $ \s@State {..} op o' ->
  case op of
    Chewable offset tr v -> case _extract offset trs chewable of
      Nothing -> error "internal error"
      Just (tr, _extracted, rest) ->
        s
          { stateRefs =
              -- so, what's going on here... we need to be able to grab and merge
              -- a list of (typeref, var). this means that we need to be able to pull
              -- out typereps symbolically too, we can't just rely on the dynamic being there.
              --                        Map.insertWith (<>) (dynTypeRep v) _  (pure o') stateRefs,
              Map.insertWith (<>) tr (_ o') stateRefs,
            chewable = rest
          }
    (Op (ApiOffset _offset) _args) ->
      s
        { chewable = _ o' : chewable
          -- Map.insertWith
          --   (<>)
          --   tr
          --   (pure o')
          -- stateRefs
        }

extract :: Int -> [a] -> Maybe (a, [a])
extract = undefined

preload :: (MonadGen n, MonadIO m) => Dynamic -> Command n m State
preload = undefined

prop_sequential :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
prop_sequential server seed = do
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
  actions <-
    forAll $ do
      Gen.sequential
        (Range.linear 1 100)
        emptyState
        (fmap preload seed <> [callEndpoint reifiedApi])
  executeSequential emptyState actions
-- prop_concurrent :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> [Dynamic] -> PropertyT IO ()
-- prop_concurrent server seed =
--   let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
--    in do
--         actions <-
--           forAll $
--             Gen.parallel
--               (Range.linear 1 50)
--               (Range.linear 1 10)
--               (emptyState {chewable = seed})
--               [callEndpoint reifiedApi]
--         test $
--           executeParallel (emptyState {chewable = seed}) actions
