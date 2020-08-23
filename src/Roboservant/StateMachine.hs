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
module Roboservant.StateMachine where

import Data.Maybe
import Servant.Client
import           Control.Arrow                      (second)
import           Control.Monad.IO.Class
import           Data.IORef                         (IORef)
-- import           Data.Dynamic                     (Dynamic, fromDynamic)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Typeable                      (TypeRep, Typeable, typeRep)
import           Roboservant.ContextualGenRequest
import           Servant.Server                     (Server)
--import           Test.QuickCheck                  (Property)
-- import           Test.StateMachine

import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty(NonEmpty)
import           Data.Dynamic
import           Data.Type.HasClass
import           Data.Type.HasClassPreludeInstances
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import Control.Monad ((<=<))
import GHC.IORef (readIORef)


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
          Nothing -> error "all screwed up"
          Just f -> runClientM f env >>= either (error . show) (pure . toDyn)

        undefined -- case fromDynamic

  in   Command gen execute
       [ Update $ \s@State{..} (Op (ApiOffset offset) args) o  ->
           s { stateRefs = Map.update (Just . (pure o <>))
               ((\(_,_,tr,_) -> tr) $ staticRoutes !! offset) stateRefs
             }
         -- , Ensure (no-500 here)
       ]
