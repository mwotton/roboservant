{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- for servant
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Roboservant.StateMachine where

import Data.Maybe
import Servant.Client
import           Control.Arrow                      (second)
import           Control.Monad.IO.Class
import           Data.IORef                         (IORef)
-- import           Data.Dynamic                     (Dynamic, fromDynamic)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Typeable                      (typeOf, TypeRep, Typeable, typeRep)
import           Roboservant.ContextualGenRequest
import           Servant.Server                     (Server)
--import           Test.QuickCheck                  (Property)
-- import           Test.StateMachine

import Hedgehog

import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty(NonEmpty)
import           Data.Dynamic
import           Data.Type.HasClass
import           Data.Type.HasClassPreludeInstances
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import Control.Monad ((<=<))
import GHC.IORef (readIORef)
import qualified Hedgehog.Range as Range

import Servant
import GHC.Generics(Generic)
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import Control.Concurrent(forkIO)
import Network.HTTP.Client(newManager, defaultManagerSettings)

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


type MyDyn = Dynamic
--  deriving Show

-- instance Eq MyDyn where
--   MyDyn a == MyDyn b = show a == show b

data State v =
  State
  { stateRefs    :: Map TypeRep (NonEmpty (Var (Opaque (IORef MyDyn)) v))
  , client :: ClientEnv
  }

type ReifiedApi = [(ApiOffset, [TypeRep], TypeRep, MyDyn)]

newtype ApiOffset = ApiOffset Int
  deriving (Eq,Show)
  deriving newtype (Enum,Num)

-- | we need to specify an offset because it's entirely possible to have two
--   functions with the same arguments that do different things.
data Op (v :: * -> * ) = Op ApiOffset [(TypeRep, Var (Opaque (IORef MyDyn)) v)]

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r op@(Op offset args) = Op offset <$> traverse (\(t,v) -> (t,) <$> htraverse r v )args

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> ClientEnv -> Command n m State
callEndpoint staticRoutes env =
  let

    gen :: MonadGen n => State Symbolic -> Maybe (n (Op Symbolic))
    gen State{..}
      | any null options = Nothing
      | otherwise = Just $ do
          uncurry Op <$> chooseOne options -- fmap Op (mapM (_ <$> Gen.element) options)

     where
       chooseOne :: MonadGen n
                 => [(ApiOffset,
                     [(TypeRep, [Var (Opaque (IORef MyDyn)) Symbolic])])]
                 -> n (ApiOffset,
                      [(TypeRep, Var (Opaque (IORef MyDyn)) Symbolic)])
       chooseOne opts = do
         (offset, args) <- Gen.element opts
         (offset,) <$> mapM (\(tr,argOpts) -> (tr,) <$> Gen.element argOpts) args


       -- | the options for each argument, in order of the parameters to the call.
       options :: [(ApiOffset, [(TypeRep, [Var (Opaque (IORef MyDyn)) Symbolic])])]
       options = mapMaybe (\(offset, argreps, retType, dynCall) ->(offset,) <$> do
                              mapM (\x -> (x,) <$> fillableWith x) argreps
                              )
                 staticRoutes

       fillableWith :: TypeRep -> Maybe [Var (Opaque (IORef MyDyn)) Symbolic]
       fillableWith tr = NEL.toList <$> Map.lookup tr stateRefs

    execute :: (Typeable a, MonadIO m)
            => Op Concrete -> m (Opaque a)
    execute (Op (ApiOffset offset) args) =
      fmap Opaque . liftIO $ do
        realArgs <- mapM (\(tr,v) -> readIORef (opaque v) ) args
        let (_offset, staticArgs,ret, endpoint) = staticRoutes !! offset
          -- now, magic happens: we apply some dynamic arguments to a dynamic
          -- function and hopefully somtehing useful pops out the end.
            func = foldr ( \arg curr ->  dynApply arg =<< curr  ) (Just endpoint) realArgs
        case func >>= fromDynamic of
          Nothing -> error ("all screwed up: " <> maybe "nothing" (show . dynTypeRep) func)
          Just (f :: ClientM a) -> runClientM f env >>= either (error . show) pure

  in   Command gen execute
       [ Update $ \s@State{..} (Op (ApiOffset offset) args) o  ->
           s { stateRefs = Map.update (Just . (pure o <>))
               ((\(_,_,tr,_) -> tr) $ staticRoutes !! offset) stateRefs
             }
         -- , Ensure (no-500 here)
       ]



prop_sm_sequential :: ReifiedApi -> ClientEnv -> Property
prop_sm_sequential reifiedApi clientEnv = do
  property $ do
    let
        initialState = State mempty clientEnv
    actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState
      [callEndpoint reifiedApi clientEnv]
    executeSequential initialState actions



newtype Foo = Foo Int
  deriving (Generic, Eq, Show)
  deriving newtype (FromHttpApiData,ToHttpApiData)

instance ToJSON Foo
instance FromJSON Foo

type FooApi
  =    "item" :> Get '[JSON] Foo
  :<|> "itemAdd" :> Capture "one" Foo :> Capture "two" Foo :> Get '[JSON] Foo
  :<|> "item" :> Capture "itemId" Foo :> Get '[JSON] ()


intro = pure (Foo 1)
combine = (\(Foo a) (Foo b) -> pure (Foo (a+b)))
eliminate = (\(Foo a) -> if a > 10
         then error "fuck"
         else pure ())

fooServer :: Server FooApi
fooServer = intro
  :<|> combine
  :<|> eliminate


runServer :: IO ()
runServer = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings (serve fooApi fooServer)

fooApi :: Proxy FooApi
fooApi = Proxy

introC :<|> combineC :<|> eliminateC = Servant.Client.client fooApi

tests :: IO Bool
tests = do
  withServantServer fooApi (pure fooServer) $ \burl -> do
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager burl
        -- faking this out for now.
        footype = typeOf Foo
        reifiedApi = [(ApiOffset 0, [],footype,toDyn introC)
                     ,(ApiOffset 1, [footype, footype],footype,toDyn combineC)
                     ,(ApiOffset 2, [footype],(typeOf ()),toDyn eliminateC)]

    -- type ReifiedApi = [(ApiOffset, [TypeRep], TypeRep, MyDyn)]
    checkParallel $ Group "props" [("aprop", prop_sm_sequential reifiedApi clientEnv )]



-- | Start a servant application on an open port, run the provided function,
-- then stop the application.
--
-- /Since 0.0.0.0/
withServantServer :: HasServer a '[] => Proxy a -> IO (Server a)
  -> (BaseUrl -> IO r) -> IO r
withServantServer api = withServantServerAndContext api EmptyContext

-- | Like 'withServantServer', but allows passing in a 'Context' to the
-- application.
--
-- /Since 0.0.0.0/
withServantServerAndContext :: (HasServer a ctx)
  => Proxy a -> Context ctx -> IO (Server a) -> (BaseUrl -> IO r) -> IO r
withServantServerAndContext api ctx server t
  = withApplication (return . serveWithContext api ctx =<< server) $ \port ->
      t (BaseUrl Http "localhost" port "")
