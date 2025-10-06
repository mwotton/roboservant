-- | Description: ways to build a reified api from a servant description.
--
--   arguably this could be more general and be abstracted away from even relying on servant
--   but that's future work.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Exception (Exception)
import Data.Dynamic (Dynamic)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import GHC.Generics ((:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Roboservant.Types.Breakdown
import Roboservant.Types.BuildFrom
import Roboservant.Types.Internal
import Servant
import Servant.API.Modifiers (FoldLenient, FoldRequired)
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Curry as V
import qualified Type.Reflection as R

newtype ApiOffset = ApiOffset Int
  deriving (Eq, Show, Ord)
  deriving newtype (Enum, Num)

type TypedF = (:*:) R.TypeRep

newtype ArgIndex = ArgIndex Int
  deriving (Eq, Show)

data PathPiece
  = StaticPiece Text
  | CapturePiece (Maybe Text) ArgIndex
  deriving (Show)

data QueryPiece
  = QueryParamPiece Text ArgIndex
  | QueryParamsPiece Text ArgIndex
  | QueryFlagPiece Text ArgIndex
  deriving (Show)

data HeaderPiece
  = HeaderPiece Text ArgIndex
  deriving (Show)

newtype BodyPiece = BodyPiece ArgIndex
  deriving (Show)

data EndpointDoc as = EndpointDoc
  { docMethod :: Text,
    docPathPieces :: [PathPiece],
    docQueryPieces :: [QueryPiece],
    docHeaderPieces :: [HeaderPiece],
    docBodyPiece :: Maybe BodyPiece
  }
  deriving (Show)

newtype Argument a = Argument
    { getArgument :: Stash -> Maybe (StashValue a)
    }

data ReifiedEndpoint = forall as. (V.RecordToList as, V.RMap as) => ReifiedEndpoint
    { reArguments    :: V.Rec (TypedF Argument) as
    , reEndpointFunc :: V.Curried as (IO (Either InteractionError (NonEmpty (Dynamic,Int))))
    , reDoc :: EndpointDoc as
    }

instance Show ReifiedEndpoint where
  show _ = "lol"

shiftIndex :: ArgIndex -> ArgIndex
shiftIndex (ArgIndex i) = ArgIndex (i + 1)

shiftPathPiece :: PathPiece -> PathPiece
shiftPathPiece = \case
  StaticPiece t -> StaticPiece t
  CapturePiece label idx -> CapturePiece label (shiftIndex idx)

shiftQueryPiece :: QueryPiece -> QueryPiece
shiftQueryPiece = \case
  QueryParamPiece name idx -> QueryParamPiece name (shiftIndex idx)
  QueryParamsPiece name idx -> QueryParamsPiece name (shiftIndex idx)
  QueryFlagPiece name idx -> QueryFlagPiece name (shiftIndex idx)

shiftHeaderPiece :: HeaderPiece -> HeaderPiece
shiftHeaderPiece (HeaderPiece name idx) = HeaderPiece name (shiftIndex idx)

shiftBodyPiece :: BodyPiece -> BodyPiece
shiftBodyPiece (BodyPiece idx) = BodyPiece (shiftIndex idx)

shiftDoc :: EndpointDoc as -> EndpointDoc (x ': as)
shiftDoc EndpointDoc {..} =
  EndpointDoc
    { docMethod = docMethod,
      docPathPieces = fmap shiftPathPiece docPathPieces,
      docQueryPieces = fmap shiftQueryPiece docQueryPieces,
      docHeaderPieces = fmap shiftHeaderPiece docHeaderPieces,
      docBodyPiece = fmap shiftBodyPiece docBodyPiece
    }

prependStaticSegment :: Text -> EndpointDoc as -> EndpointDoc as
prependStaticSegment seg doc =
  doc {docPathPieces = StaticPiece seg : docPathPieces doc}

prependCaptureSegment :: Maybe Text -> EndpointDoc as -> EndpointDoc (a ': as)
prependCaptureSegment label doc =
  let shifted = shiftDoc doc
   in shifted {docPathPieces = CapturePiece label (ArgIndex 0) : docPathPieces shifted}

prependQueryParam :: Text -> EndpointDoc as -> EndpointDoc (a ': as)
prependQueryParam name doc =
  let shifted = shiftDoc doc
   in shifted {docQueryPieces = docQueryPieces shifted ++ [QueryParamPiece name (ArgIndex 0)]}

prependQueryParams :: Text -> EndpointDoc as -> EndpointDoc (a ': as)
prependQueryParams name doc =
  let shifted = shiftDoc doc
   in shifted {docQueryPieces = docQueryPieces shifted ++ [QueryParamsPiece name (ArgIndex 0)]}

prependQueryFlag :: Text -> EndpointDoc as -> EndpointDoc (a ': as)
prependQueryFlag name doc =
  let shifted = shiftDoc doc
   in shifted {docQueryPieces = docQueryPieces shifted ++ [QueryFlagPiece name (ArgIndex 0)]}

prependHeader :: Text -> EndpointDoc as -> EndpointDoc (a ': as)
prependHeader name doc =
  let shifted = shiftDoc doc
   in shifted {docHeaderPieces = docHeaderPieces shifted ++ [HeaderPiece name (ArgIndex 0)]}

setRequestBody :: EndpointDoc as -> EndpointDoc (a ': as)
setRequestBody doc =
  let shifted = shiftDoc doc
   in shifted {docBodyPiece = Just (BodyPiece (ArgIndex 0))}

class ( V.RecordToList (EndpointArgs endpoint)
      , V.RMap (EndpointArgs endpoint)
      ) => ToReifiedEndpoint (endpoint :: Type) where
  type EndpointArgs endpoint :: [Type]
  type EndpointRes endpoint :: Type

  reifiedEndpointArguments :: V.Rec (TypedF Argument) (EndpointArgs endpoint)
  reifiedEndpointDoc :: EndpointDoc (EndpointArgs endpoint)


tagType :: Typeable a => f a -> TypedF f a
tagType = (R.typeRep :*:)

data InteractionError = InteractionError
  { errorMessage :: T.Text
  , fatalError :: Bool
  }
  deriving Show
instance Exception InteractionError



instance
  ( Typeable responseType
  , Breakdown responseType
  , ReflectMethod method
  ) =>
  ToReifiedEndpoint (Verb method statusCode contentTypes responseType)
  where
  type EndpointArgs (Verb method statusCode contentTypes responseType) = '[]
  type EndpointRes (Verb method statusCode contentTypes responseType) = responseType
  reifiedEndpointArguments = V.RNil
  reifiedEndpointDoc =
    EndpointDoc
      { docMethod = TE.decodeUtf8 (reflectMethod (Proxy @method)),
        docPathPieces = [],
        docQueryPieces = [],
        docHeaderPieces = [],
        docBodyPiece = Nothing
      }

instance (ReflectMethod method) => ToReifiedEndpoint (NoContentVerb method)
  where
  type EndpointArgs (NoContentVerb method) = '[]
  type EndpointRes (NoContentVerb method) = NoContent
  reifiedEndpointArguments = V.RNil
  reifiedEndpointDoc =
    EndpointDoc
      { docMethod = TE.decodeUtf8 (reflectMethod (Proxy @method)),
        docPathPieces = [],
        docQueryPieces = [],
        docHeaderPieces = [],
        docBodyPiece = Nothing
      }

instance
  (KnownSymbol x, ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint ((x :: Symbol) :> endpoint)
  where
  type EndpointArgs ((x :: Symbol) :> endpoint) = EndpointArgs endpoint
  type EndpointRes ((x :: Symbol) :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependStaticSegment (T.pack (symbolVal (Proxy @x))) (reifiedEndpointDoc @endpoint)

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (RemoteHost :> endpoint)
  where
  type EndpointArgs (RemoteHost :> endpoint) = EndpointArgs endpoint
  type EndpointRes (RemoteHost :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint
  reifiedEndpointDoc = reifiedEndpointDoc @endpoint



instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Description s :> endpoint)
  where
  type EndpointArgs (Description s :> endpoint) = EndpointArgs endpoint
  type EndpointRes (Description s :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint
  reifiedEndpointDoc = reifiedEndpointDoc @endpoint

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Summary s :> endpoint)
  where
  type EndpointArgs (Summary s :> endpoint) = EndpointArgs endpoint
  type EndpointRes (Summary s :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = reifiedEndpointArguments @endpoint
  reifiedEndpointDoc = reifiedEndpointDoc @endpoint

instance
  ( Typeable requestType
  , BuildFrom requestType
  , ToReifiedEndpoint endpoint
  , KnownSymbol name
  ) =>
  ToReifiedEndpoint (QueryFlag name :> endpoint)
  where
  type EndpointArgs (QueryFlag name :> endpoint) = Bool ': EndpointArgs endpoint
  type EndpointRes (QueryFlag name :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments = tagType (Argument (buildFrom @Bool)) V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependQueryFlag (T.pack (symbolVal (Proxy @name))) (reifiedEndpointDoc @endpoint)

type IfLenient s mods t  = If (FoldLenient mods) (Either s t) t
type IfRequired mods t = If (FoldRequired mods) t (Maybe t)
type IfRequiredLenient s mods t = IfRequired mods (IfLenient s mods t)

instance
  ( BuildFrom (IfRequiredLenient T.Text mods paramType)
  , ToReifiedEndpoint endpoint
  , KnownSymbol name
  ) =>
  ToReifiedEndpoint (QueryParam' mods name paramType :> endpoint)
  where
  type EndpointArgs (QueryParam' mods name paramType :> endpoint) = IfRequiredLenient T.Text mods paramType ': EndpointArgs endpoint
  type EndpointRes (QueryParam' mods name paramType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfRequiredLenient T.Text mods paramType)))
      V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependQueryParam (T.pack (symbolVal (Proxy @name))) (reifiedEndpointDoc @endpoint)


instance
  ( BuildFrom paramType
  , ToReifiedEndpoint endpoint
  , Show paramType
  , Eq paramType
  , KnownSymbol name
  ) =>
  ToReifiedEndpoint (QueryParams name paramType :> endpoint)
  where
  type EndpointArgs (QueryParams name paramType :> endpoint) =  [paramType] ': EndpointArgs endpoint
  type EndpointRes (QueryParams name paramType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
    tagType (Argument (buildFrom @[paramType]))
      V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependQueryParams (T.pack (symbolVal (Proxy @name))) (reifiedEndpointDoc @endpoint)




instance
  ( BuildFrom (IfRequiredLenient T.Text mods headerType)
  , ToReifiedEndpoint endpoint
  , KnownSymbol headerName
  ) =>
  ToReifiedEndpoint (Header' mods headerName headerType :> endpoint)
  where
  type EndpointArgs (Header' mods headerName headerType :> endpoint) = IfRequiredLenient T.Text mods headerType ': EndpointArgs endpoint
  type EndpointRes  (Header' mods headerName headerType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfRequiredLenient T.Text mods headerType)))
      V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependHeader (T.pack (symbolVal (Proxy @headerName))) (reifiedEndpointDoc @endpoint)

#if MIN_VERSION_servant(0,17,0)
instance
  ( BuildFrom (IfLenient String mods captureType)
  , ToReifiedEndpoint endpoint
  , KnownSymbol name
  ) =>
  ToReifiedEndpoint (Capture' mods name captureType :> endpoint)
  where
  type EndpointArgs (Capture' mods name captureType :> endpoint) = IfLenient String mods captureType ': EndpointArgs endpoint
  type EndpointRes  (Capture' mods name captureType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(IfLenient String mods captureType)))
      V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependCaptureSegment (Just (T.pack (symbolVal (Proxy @name)))) (reifiedEndpointDoc @endpoint)
#else
instance
  ( BuildFrom captureType
  , ToReifiedEndpoint endpoint
  , KnownSymbol name
  ) =>
  ToReifiedEndpoint (Capture' mods name captureType :> endpoint)
  where
  type EndpointArgs (Capture' mods name captureType :> endpoint) = captureType ': EndpointArgs endpoint
  type EndpointRes  (Capture' mods name captureType :> endpoint) = EndpointRes endpoint
  reifiedEndpointArguments =
   tagType (Argument (buildFrom @(captureType)))
      V.:& reifiedEndpointArguments @endpoint
  reifiedEndpointDoc =
    prependCaptureSegment (Just (T.pack (symbolVal (Proxy @name)))) (reifiedEndpointDoc @endpoint)

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
  reifiedEndpointDoc =
    setRequestBody (reifiedEndpointDoc @endpoint)
