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
import           Data.ConstrainedDynamic
import qualified Data.MultiConstrainedDynamic       as MCD
import           Data.Type.HasClass
import           Data.Type.HasClassPreludeInstances
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen


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


newtype MyDyn = MyDyn (ConstrainedDynamic Show)
  deriving Show

instance Eq MyDyn where
  MyDyn a == MyDyn b = show a == show b

data State v =
  State
  { stateRefs    :: Map TypeRep (NonEmpty (Var (Opaque (IORef MyDyn)) v))
  } deriving (Show)

type ReifiedApi = [([TypeRep], TypeRep, MyDyn)]

-- newtype ApiOffset = ApiOffset Int
--   deriving (Eq,Show)
--   deriving newtype (Enum,Num)
data Op (v :: * -> * ) = Op [(TypeRep, Var (Opaque MyDyn) v)]

deriving instance Show (Op Symbolic)

instance HTraversable Op where
  htraverse r op@(Op args) = Op <$> traverse (\(t,v) -> (t,) <$> htraverse r v )args

callEndpoint :: (MonadGen n, MonadIO m) => ReifiedApi -> ClientEnv -> Command n m State
callEndpoint staticRoutes env =
  let

    gen :: MonadGen n => State Symbolic -> Maybe (n (Op Symbolic))
    gen State{..}
      | any null options = Nothing
      | otherwise = Just $ (mapM Gen.element options >>= pure . Op . _) -- fmap Op (mapM (_ <$> Gen.element) options)

     where
       options = map (\(args,typerep,_) -> map selectByType args) staticRoutes
       -- | given a reified type sig for an operation, work out what
       --   options there are for each
       selectByType typerep = case Map.lookup typerep stateRefs of
         Nothing -> []
         Just xs -> fmap (typerep,) (NEL.toList xs)


    execute (Op args) =
      fmap Opaque . liftIO $ undefined -- case fromDynamic

  in  Command gen execute [ Update $ undefined
                           -- , Ensure (no-500 here)
                          ]


  --     -- [
--     --     Update $ \(State xs) _i o ->
--     --       State $
--     --         xs ++ [(o, 0)]
--     --   ]
