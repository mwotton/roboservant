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

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic (Dynamic, dynApply, dynTypeRep, fromDynamic, toDyn)
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.IORef (readIORef)
import GHC.TypeLits (Symbol)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Servant
import Type.Reflection (SomeTypeRep)

data State v
  = State
      { stateRefs :: Map TypeRep (NonEmpty (Var (Opaque (IORef Dynamic)) v))
      }

class FlattenServer api where
  flattenServer :: Server api -> Bundled (Endpoints api)

instance
  ( Endpoints (endpoint :<|> api) ~ (endpoint ': Endpoints api),
    Server (endpoint :<|> api) ~ (Server endpoint :<|> Server api),
    FlattenServer api
  ) =>
  FlattenServer (endpoint :<|> api)
  where
  flattenServer (endpoint :<|> server) = endpoint `AnEndpoint` flattenServer @api server

instance
  ( HasServer (x :> api) '[],
    Endpoints (x :> api) ~ '[x :> api]
  ) =>
  FlattenServer (x :> api)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

instance
  ( HasServer (Verb method statusCode contentTypes responseType) '[],
    Endpoints (Verb method statusCode contentTypes responseType) ~ '[Verb method statusCode contentTypes responseType]
  ) =>
  FlattenServer (Verb method statusCode contentTypes responseType)
  where
  flattenServer server = server `AnEndpoint` NoEndpoints

type ReifiedEndpoint = ([TypeRep], TypeRep, Dynamic)

type ReifiedApi = [(ApiOffset, [TypeRep], TypeRep, Dynamic)]

data Bundled endpoints where
  AnEndpoint :: Server endpoint -> Bundled endpoints -> Bundled (endpoint ': endpoints)
  NoEndpoints :: Bundled '[]

class ToReifiedApi (endpoints :: [*]) where
  toReifiedApi :: Bundled endpoints -> Proxy endpoints -> ReifiedApi

class ToReifiedEndpoint (endpoint :: *) where
  toReifiedEndpoint :: Dynamic -> Proxy endpoint -> ReifiedEndpoint

instance ToReifiedApi '[] where
  toReifiedApi NoEndpoints _ = []

instance
  (Typeable (Normal (ServerT endpoint Handler)), NormalizeFunction (ServerT endpoint Handler), ToReifiedEndpoint endpoint, ToReifiedApi endpoints, Typeable (ServerT endpoint Handler)) =>
  ToReifiedApi (endpoint : endpoints)
  where
  toReifiedApi (endpoint `AnEndpoint` endpoints) _ =
    withOffset (toReifiedEndpoint (toDyn (normalize endpoint)) (Proxy @endpoint))
      : map
        (\(n, x, y, z) -> (n + 1, x, y, z))
        (toReifiedApi endpoints (Proxy @endpoints))
    where
      withOffset (x, y, z) = (0, x, y, z)

class NormalizeFunction m where
  type Normal m
  normalize :: m -> Normal m

instance NormalizeFunction x => NormalizeFunction (r -> x) where
  type Normal (r -> x) = r -> Normal x
  normalize = fmap normalize

instance Typeable x => NormalizeFunction (Handler x) where
  type Normal (Handler x) = IO (Either ServerError (TypeRep, Dynamic))
  normalize handler = (runExceptT . runHandler') handler >>= \case
    Left serverError -> pure (Left serverError)
    Right x -> pure (Right (typeRep (Proxy @x), toDyn x))

instance
  Typeable responseType =>
  ToReifiedEndpoint (Verb method statusCode contentTypes responseType)
  where
  toReifiedEndpoint endpoint _ =
    ([], typeRep (Proxy @responseType), endpoint)

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint ((x :: Symbol) :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)

instance
  (Typeable requestType, ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Capture name requestType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @requestType) : args, result, typeRepMap)

instance
  (Typeable requestType, ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (ReqBody contentTypes requestType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @requestType) : args, result, typeRepMap)

newtype ApiOffset = ApiOffset Int
  deriving (Eq, Show)
  deriving newtype (Enum, Num)

-- | we need to specify an offset because it's entirely possible to have two
--   functions with the same arguments that do different things.
data Op (v :: * -> *) = Op ApiOffset [(TypeRep, Var (Opaque (IORef Dynamic)) v)]

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r (Op offset args) = Op offset <$> traverse (\(t, v) -> (t,) <$> htraverse r v) args

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> Command n m State
callEndpoint staticRoutes =
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
            (offset, args) <- Gen.element opts
            (offset,) <$> mapM (\(tr, argOpts) -> (tr,) <$> Gen.element argOpts) args
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
      execute (Op (ApiOffset offset) args) = do
        --        traceM (show (offset, args))
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
        [ Update $ \s@State {..} (Op (ApiOffset offset) _args) o' ->
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

prop_sequential :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> Property
prop_sequential server = do
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api))
  property $ do
    let initialState = State mempty
    actions <-
      forAll $
        Gen.sequential
          (Range.linear 1 100)
          initialState
          [callEndpoint reifiedApi]
    executeSequential initialState actions

prop_concurrent :: forall api. (FlattenServer api, ToReifiedApi (Endpoints api)) => Server api -> Property
prop_concurrent server =
  let reifiedApi = toReifiedApi (flattenServer @api server) (Proxy @(Endpoints api)) in
  let initialState = State mempty
   in withTests 1000 . withRetries 10 . property $ do
        actions <-
          forAll $
            Gen.parallel
              (Range.linear 1 50)
              (Range.linear 1 10)
              initialState
              [callEndpoint reifiedApi]
        test $
          executeParallel initialState actions
