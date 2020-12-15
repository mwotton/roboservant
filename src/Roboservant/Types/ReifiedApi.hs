
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

module Roboservant.Types.ReifiedApi where


import Control.Monad.Except (runExceptT)
import Data.Bifunctor
-- import Data.Dependent.Sum
import Data.Dynamic (Dynamic)
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import GHC.Generics ((:*:)(..))
import GHC.TypeLits (Symbol)
import Roboservant.Types.Internal
import Roboservant.Types.Breakdown
import Roboservant.Types.BuildFrom
import Roboservant.Types.FlattenServer
import Servant
import Servant.API.Modifiers(FoldRequired,FoldLenient)
import qualified Data.Text as T
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Curry as V
import qualified Type.Reflection as R


newtype ApiOffset = ApiOffset Int
  deriving (Eq, Show, Ord)
  deriving newtype (Enum, Num)

type TypedF = (:*:) R.TypeRep

newtype Argument a = Argument
    { getArgument :: Stash -> Maybe (StashValue a)
    }

data ReifiedEndpoint = forall as. (V.RecordToList as, V.RMap as) => ReifiedEndpoint
    { reArguments    :: V.Rec (TypedF Argument) as
    , reEndpointFunc :: V.Curried as (IO (Either ServerError (NonEmpty (Dynamic,Int))))
    }

type ReifiedApi = [(ApiOffset, ReifiedEndpoint)]

tagType :: Typeable a => f a -> TypedF f a
tagType = (R.typeRep :*:)

class ToReifiedApi (endpoints :: [*]) where
  toReifiedApi :: Bundled endpoints -> Proxy endpoints -> ReifiedApi

class ( V.Curried (EndpointArgs endpoint) (Handler (EndpointRes endpoint)) ~ Server endpoint
      , V.RecordToList (EndpointArgs endpoint)
      , V.RMap (EndpointArgs endpoint)
      ) => ToReifiedEndpoint (endpoint :: *) where
  type EndpointArgs endpoint :: [Type]
  type EndpointRes endpoint :: Type

  reifiedEndpointArguments :: V.Rec (TypedF Argument) (EndpointArgs endpoint)

instance ToReifiedApi '[] where
  toReifiedApi NoEndpoints _ = []

instance
  ( Typeable (EndpointRes endpoint)
  , NormalizeFunction (ServerT endpoint Handler)
  , Normal (ServerT endpoint Handler) ~ V.Curried (EndpointArgs endpoint) (IO (Either ServerError (NonEmpty (Dynamic,Int))))
  , ToReifiedEndpoint endpoint
  , ToReifiedApi endpoints, Typeable (ServerT endpoint Handler)
  ) =>
  ToReifiedApi (endpoint : endpoints)
  where
  toReifiedApi (endpoint `AnEndpoint` endpoints) _ =
    (0, ReifiedEndpoint
         { reArguments    = reifiedEndpointArguments @endpoint
         , reEndpointFunc = normalize endpoint
         }
    )
      : (map . first) (+1)
        (toReifiedApi endpoints (Proxy @endpoints))

class NormalizeFunction m where
  type Normal m
  normalize :: m -> Normal m

instance NormalizeFunction x => NormalizeFunction (r -> x) where
  type Normal (r -> x) = r -> Normal x
  normalize = fmap normalize

instance (Typeable x, Breakdown x) => NormalizeFunction (Handler x) where
  type Normal (Handler x) = IO (Either ServerError (NonEmpty (Dynamic,Int)))
  normalize handler = (runExceptT . runHandler') handler >>= \case
    Left serverError -> pure (Left serverError)
    Right x -> pure $ Right $ breakdown x

instance
  (Typeable responseType, Breakdown responseType) =>
  ToReifiedEndpoint (Verb method statusCode contentTypes responseType)
  where
  type EndpointArgs (Verb method statusCode contentTypes responseType) = '[]
  type EndpointRes (Verb method statusCode contentTypes responseType) = responseType
  reifiedEndpointArguments = V.RNil

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint ((x :: Symbol) :> endpoint)
  where
  type EndpointArgs ((x :: Symbol) :> endpoint) = EndpointArgs endpoint
  type EndpointRes ((x :: Symbol) :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Description s :> endpoint)
  where
  type EndpointArgs (Description s :> endpoint) = EndpointArgs endpoint
  type EndpointRes (Description s :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Summary s :> endpoint)
  where
  type EndpointArgs (Summary s :> endpoint) = EndpointArgs endpoint
  type EndpointRes (Summary s :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint

instance
  (Typeable requestType
  ,BuildFrom requestType
  ,ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (QueryFlag name :> endpoint)
  where
  type EndpointArgs (QueryFlag name :> endpoint) = Bool ': EndpointArgs endpoint
  type EndpointRes (QueryFlag name :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = tagType (Argument (buildFrom @Bool)) V.:& reifiedEndpointArguments @endpoint

type IfLenient s mods t  = If (FoldLenient mods) (Either s t) t
type IfRequired mods t = If (FoldRequired mods) t (Maybe t)
type IfRequiredLenient s mods t = IfRequired mods (IfLenient s mods t)

instance
  ( BuildFrom (IfRequiredLenient T.Text mods paramType)
  , ToReifiedEndpoint endpoint
  ) =>
  ToReifiedEndpoint (QueryParam' mods name paramType :> endpoint)
  where
  type EndpointArgs (QueryParam' mods name paramType :> endpoint) = IfRequiredLenient T.Text mods paramType ': EndpointArgs endpoint
  type EndpointRes (QueryParam' mods name paramType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfRequiredLenient T.Text mods paramType)))
      V.:& reifiedEndpointArguments @endpoint


instance
  ( BuildFrom (IfRequiredLenient T.Text mods headerType)
  , ToReifiedEndpoint endpoint
  ) =>
  ToReifiedEndpoint (Header' mods headerName headerType :> endpoint)
  where
  type EndpointArgs (Header' mods headerName headerType :> endpoint) = IfRequiredLenient T.Text mods headerType ': EndpointArgs endpoint
  type EndpointRes  (Header' mods headerName headerType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfRequiredLenient T.Text mods headerType)))
      V.:& reifiedEndpointArguments @endpoint


-- this isn't happy in 0.16.2
instance
  ( BuildFrom (IfLenient String mods captureType)
  , ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Capture' mods name captureType :> endpoint)
  where
  type EndpointArgs (Capture' mods name captureType :> endpoint) = IfLenient String mods captureType ': EndpointArgs endpoint
  type EndpointRes  (Capture' mods name captureType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfLenient String mods captureType)))
      V.:& reifiedEndpointArguments @endpoint

instance
  ( BuildFrom (IfLenient String mods requestType)
  , ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (ReqBody' mods contentTypes requestType :> endpoint)
  where
  type EndpointArgs (ReqBody' mods contentTypes requestType :> endpoint) = IfLenient String mods requestType ': EndpointArgs endpoint
  type EndpointRes  (ReqBody' mods contentTypes requestType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfLenient String mods requestType)))
      V.:& reifiedEndpointArguments @endpoint
