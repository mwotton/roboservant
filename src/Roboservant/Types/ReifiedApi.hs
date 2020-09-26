
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant.Types.ReifiedApi where


import Control.Monad.Except (runExceptT)
import Data.Dynamic (Dynamic, toDyn)
import Data.Function ((&))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.TypeLits (Symbol)
import Servant
import Servant.API.Modifiers(FoldRequired,FoldLenient)
import Roboservant.Types.FlattenServer


newtype ApiOffset = ApiOffset Int
  deriving (Eq, Show)
  deriving newtype (Enum, Num)

type ReifiedEndpoint = ([TypeRep], TypeRep, Dynamic)

type ReifiedApi = [(ApiOffset, [TypeRep], TypeRep, Dynamic)]


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
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Description s :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)

instance
  (ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Summary s :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)

instance
  (Typeable requestType, ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (QueryFlag name :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @Bool) : args, result, typeRepMap)

instance
  (Typeable (If (FoldRequired mods) paramType (Maybe paramType)), ToReifiedEndpoint endpoint, SBoolI (FoldRequired mods)) =>
  ToReifiedEndpoint (QueryParam' mods name paramType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @(If (FoldRequired mods) paramType (Maybe paramType))) : args, result, typeRepMap)

instance
  (Typeable (If (FoldRequired mods) headerType (Maybe headerType)), ToReifiedEndpoint endpoint, SBoolI (FoldRequired mods)) =>
  ToReifiedEndpoint (Header' mods headerName headerType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @(If (FoldRequired mods) headerType (Maybe headerType))) : args, result, typeRepMap)

instance
  (Typeable captureType, ToReifiedEndpoint endpoint) =>
  ToReifiedEndpoint (Capture' mods name captureType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @captureType) : args, result, typeRepMap)

instance
  (Typeable (If (FoldLenient mods) (Either String requestType) requestType), ToReifiedEndpoint endpoint, SBoolI (FoldLenient mods)) =>
  ToReifiedEndpoint (ReqBody' mods contentTypes requestType :> endpoint)
  where
  toReifiedEndpoint endpoint _ =
    toReifiedEndpoint endpoint (Proxy @endpoint)
      & \(args, result, typeRepMap) -> (typeRep (Proxy @(If (FoldLenient mods) (Either String requestType) requestType)) : args, result, typeRepMap)
