{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- for servant

module Roboservant.StateMachine where

import Control.Arrow (second)
-- import           Data.Dynamic                     (Dynamic, fromDynamic)

--import           Test.QuickCheck                  (Property)
-- import           Test.StateMachine

import Type.Reflection (SomeTypeRep)
import Servant.API.Flatten (Flat)
import GHC.TypeLits (Nat, Symbol)
import Data.Function ((&))
import Control.Concurrent (forkIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Dynamic
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Type.HasClass
import Data.Type.HasClassPreludeInstances
import Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import Debug.Trace (traceM, traceShowM)
import GHC.Generics (Generic)
import GHC.Generics
import GHC.IORef (readIORef)
import Control.Monad.Except (runExceptT)
import Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Roboservant.ContextualGenRequest
import Servant
import Servant.Client
import Servant.Server (Server)

-- | can then use either `forAllParallelCommands` or `forAllCommands` to turn
--   this into a property
--
-- TODO: how do we tie the model, command and response types to the Server?
-- genStateMachine :: HasContextualGenRequest a => Server a -> StateMachine Model Command IO Response
-- genStateMachine =  StateMachine initModel _transition _precondition _postcondition
--                    Nothing _generator _shrinker _semantics _mock

-- initModel :: Model r
-- initModel = Model mempty

-- newtype Model r = Model (Map TypeRep Dynamic)

-- data Command r
--   = CallEndpoint
--   | Read      (Reference (Opaque (IORef Int)) r)
--   | Write     (Reference (Opaque (IORef Int)) r) Int
--   | Increment (Reference (Opaque (IORef Int)) r)

-- data Response r
--   = Created (Reference (Opaque (IORef Int)) r)
--   | ReadValue Int
--   | Written
--   | Incremented

--  deriving Show

-- instance Eq MyDyn where
--   MyDyn a == MyDyn b = show a == show b

data State v
  = State
      { stateRefs :: Map TypeRep (NonEmpty (Var (Opaque (IORef Dynamic)) v))
      }

class FlattenServer api where
  flattenServer :: Server api -> Bundled (Endpoints api)

instance
  ( Endpoints (endpoint :<|> api) ~ (endpoint ': Endpoints api)
  , Server (endpoint :<|> api) ~ (Server endpoint :<|> Server api)
  , FlattenServer api
  ) => FlattenServer (endpoint :<|> api) where
  flattenServer (endpoint :<|> server) = endpoint `AnEndpoint` flattenServer @api server

instance
  ( HasServer (x :> api) '[]
  , Endpoints (x :> api) ~ '[x :> api]
  ) => FlattenServer (x :> api) where
  flattenServer server = server `AnEndpoint` NoEndpoints

instance
  ( HasServer (Verb method statusCode contentTypes responseType) '[],
    Endpoints (Verb method statusCode contentTypes responseType) ~ '[Verb method statusCode contentTypes responseType]
  ) => FlattenServer (Verb method statusCode contentTypes responseType) where
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

instance (Typeable (Normal (ServerT endpoint Handler)), NormalizeFunction (ServerT endpoint Handler), ToReifiedEndpoint endpoint, ToReifiedApi endpoints, Typeable (ServerT endpoint Handler))
  => ToReifiedApi (endpoint : endpoints)
  where
    toReifiedApi (endpoint `AnEndpoint` endpoints) _ =
      withOffset (toReifiedEndpoint (toDyn (normalize endpoint)) (Proxy @endpoint))
        : map (\(n, x, y, z) -> (n + 1, x, y, z))
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
    
instance Typeable responseType
  => ToReifiedEndpoint (Verb method statusCode contentTypes responseType)
  where
    toReifiedEndpoint endpoint _ =
      ([], typeRep (Proxy @responseType), endpoint)

instance (ToReifiedEndpoint endpoint)
  => ToReifiedEndpoint ((x :: Symbol) :> endpoint)
  where
    toReifiedEndpoint endpoint _ =
      toReifiedEndpoint endpoint (Proxy @endpoint) 

instance (Typeable requestType, ToReifiedEndpoint endpoint)
  => ToReifiedEndpoint (Capture name requestType :> endpoint)
  where
    toReifiedEndpoint endpoint _ =
      toReifiedEndpoint endpoint (Proxy @endpoint) 
        & \(args, result, typeRepMap) -> (typeRep (Proxy @requestType) : args, result, typeRepMap)

instance (Typeable requestType, ToReifiedEndpoint endpoint)
  => ToReifiedEndpoint (ReqBody contentTypes requestType :> endpoint)
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
  htraverse r op@(Op offset args) = Op offset <$> traverse (\(t, v) -> (t,) <$> htraverse r v) args

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
              ( \(offset, argreps, retType, dynCall) -> (offset,) <$> do
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
          realArgs <- mapM (\(tr, v) -> readIORef (opaque v)) args
          let (_offset, staticArgs, ret, endpoint) = staticRoutes !! offset
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
                  Right (typeRep :: SomeTypeRep, (dyn :: Dynamic)) -> newIORef dyn
   in Command
        gen
        execute
        [ Update $ \s@State {..} (Op (ApiOffset offset) args) o' ->
            --            let foo :: Var (Opaque Dynamic) v -> Var (Opaque (IORef Dynamic)) v
            --                foo = _ . opaque
            --             in
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

prop_sm_sequential :: ReifiedApi -> Property
prop_sm_sequential reifiedApi = do
  property $ do
    let initialState = State mempty
    actions <-
      forAll $
        Gen.sequential
          (Range.linear 1 100)
          initialState
          [callEndpoint reifiedApi]
    executeSequential initialState actions

newtype Foo = Foo Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance ToJSON Foo

instance FromJSON Foo

type FooApi =
  "item" :> Get '[JSON] Foo
    :<|> "itemAdd" :> Capture "one" Foo :> Capture "two" Foo :> Get '[JSON] Foo
    :<|> "item" :> Capture "itemId" Foo :> Get '[JSON] ()

intro = pure (Foo 1)

combine = (\(Foo a) (Foo b) -> pure (Foo (a + b)))

eliminate =
  ( \(Foo a) ->
      if a > 10
        then error "eliminate blew up, oh no!"
        else pure ()
  )

fooServer :: Server FooApi
fooServer =
  intro
    :<|> combine
    :<|> eliminate

bundledFooServer :: Bundled (Endpoints FooApi)
bundledFooServer = flattenServer @FooApi fooServer

runServer :: IO ()
runServer = do
  let port = 3000
      settings =
        setPort port
          $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port))
          $ defaultSettings
  runSettings settings (serve fooApi fooServer)

fooApi :: Proxy FooApi
fooApi = Proxy

introC :<|> combineC :<|> eliminateC = Servant.Client.client fooApi

tests :: IO Bool
tests = do
  let reifiedApi = toReifiedApi (flattenServer @FooApi fooServer) (Proxy @(Endpoints FooApi))
  checkParallel $ Group "props" [("aprop", prop_sm_sequential reifiedApi)]
