-- | Description: ways to build a reified api from a servant description.
--
--   arguably this could be more general and be abstracted away from even relying on servant
--   but that's future work.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Roboservant.Types.ReifiedApi where

import Data.Dynamic (Dynamic)
import Control.Exception(Exception)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import GHC.Generics ((:*:)(..))
import Roboservant.Types.Internal
import Roboservant.Types.Breakdown
import Roboservant.Types.BuildFrom
import Data.Kind(Type)
import Servant
import Servant.API.Modifiers(FoldRequired,FoldLenient)
import GHC.TypeLits (Symbol)
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
    , reEndpointFunc :: V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic,Int))))
    }

instance Show ReifiedEndpoint where
  show _ = "lol"

class ( V.RecordToList (EndpointArgs endpoint)
      , V.RMap (EndpointArgs endpoint)
      ) => ToReifiedEndpoint (endpoint :: Type) where
  type EndpointArgs endpoint :: [Type]
  type EndpointRes endpoint :: Type

  reifiedEndpointArguments :: V.Rec (TypedF Argument) (EndpointArgs endpoint)


tagType :: Typeable a => f a -> TypedF f a
tagType = (R.typeRep :*:)

data InteractionError = InteractionError
  { errorMessage :: T.Text
  , fatalError :: Bool
  }
  deriving Show
instance Exception InteractionError



instance
  (Typeable responseType, Breakdown responseType) =>
  ToReifiedEndpoint (Verb method statusCode contentTypes responseType)
  where
  type EndpointArgs (Verb method statusCode contentTypes responseType) = '[]
  type EndpointRes (Verb method statusCode contentTypes responseType) = responseType
  reifiedEndpointArguments = V.RNil

instance ToReifiedEndpoint (NoContentVerb method)
  where
  type EndpointArgs (NoContentVerb method) = '[]
  type EndpointRes (NoContentVerb method) = NoContent
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
  ToReifiedEndpoint (RemoteHost :> endpoint)
  where
  type EndpointArgs (RemoteHost :> endpoint) = EndpointArgs endpoint
  type EndpointRes (RemoteHost :> endpoint) = EndpointRes endpoint
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
  ( BuildFrom paramType
  , ToReifiedEndpoint endpoint
  , Show paramType
  , Eq paramType
  ) =>
  ToReifiedEndpoint (QueryParams name paramType :> endpoint)
  where
  type EndpointArgs (QueryParams name paramType :> endpoint) =  [paramType] ': EndpointArgs endpoint
  type EndpointRes (QueryParams name paramType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
    tagType (Argument (buildFrom @[paramType]))
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

#if MIN_VERSION_servant(0,17,0)
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
#else
instance
  ( BuildFrom captureType
  , ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Capture' mods name captureType :> endpoint)
  where
  type EndpointArgs (Capture' mods name captureType :> endpoint) = captureType ': EndpointArgs endpoint
  type EndpointRes  (Capture' mods name captureType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(captureType)))
      V.:& reifiedEndpointArguments @endpoint

#endif

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
