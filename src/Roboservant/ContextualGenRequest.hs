{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Roboservant.ContextualGenRequest where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)
import           Data.Data                (Proxy (Proxy))
import           Data.String              (IsString (fromString))
import           Data.String.Conversions  (cs)
import           GHC.TypeLits             (KnownSymbol, Nat, symbolVal)
import           Network.HTTP.Client      (Request,
                                           RequestBody (RequestBodyLBS),
                                           defaultRequest, host, method, path,
                                           port, queryString, requestBody,
                                           requestHeaders, secure)
import           Network.HTTP.Media       (renderHeader)
import           Servant
import           Servant.API
import           Servant.API.ContentTypes (AllMimeRender, allMimeRender)
import           Servant.Client
-- import           Test.QuickCheck          (Arbitrary, Gen, elements, frequency)
import           Data.Dynamic             (Dynamic)
import           Data.Map.Strict          (Map)
import           Data.Maybe
import           Data.Typeable            (TypeRep)
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import           Roboservant.Hedgehog

class HasContextualGenRequest a where
  genContextualRequest :: Proxy a -> Map TypeRep Dynamic -> Maybe (Int, Gen ( BaseUrl -> Request))

-- | Generatable is functionally equivalent to Arbitrary with a default.
--   It is separate because we want to allow overwriting at will.
class Generatable a where
  generator :: Gen [a]
  generator = pure []

instance ( HasContextualGenRequest a
         , HasContextualGenRequest b)
  => HasContextualGenRequest  (a :<|> b) where
    genContextualRequest _ store
      = case sub of
          [] -> Nothing
          _  -> Just (newfreq, Gen.frequency sub)
      where
        newfreq = sum (map fst sub)
        sub = catMaybes [l,r]
        l = genContextualRequest (Proxy :: Proxy a) store
        r = genContextualRequest (Proxy :: Proxy b) store

withGeneratable p store cont = cont <$> genContextualRequest p store

instance (KnownSymbol path, HasContextualGenRequest b )
  => HasContextualGenRequest (path :> b) where
    genContextualRequest _ store =
      withGeneratable (Proxy :: Proxy b) store $ \(oldf,old') -> do
        ( oldf, do
            old <- old'
            pure $ \burl ->
              let r = old burl
                  oldPath = path r
                  oldPath' = BS.dropWhile (== BS.c2w '/') oldPath
                  paths = filter (not . BS.null) [new, oldPath']
              in r { path = "/" <> BS.intercalate "/" paths }

          )
--        , pure ( \burl -> do
--            _
        -- fmap (Just . (1,)) $ return $ \burl st ->
      where
        -- (oldf, old) = genContextualRequest  (Proxy :: Proxy b) store
        new = cs $ symbolVal (Proxy :: Proxy path)

-- instance HasContextualGenRequest EmptyAPI st where
--   genContextualRequest _ = (0, error "EmptyAPIs cannot be queried.")

-- instance HasContextualGenRequest api st => HasContextualGenRequest (Summary d :> api) st where
--   genContextualRequest _ = genContextualRequest (Proxy :: Proxy api)

-- instance HasContextualGenRequest api st => HasContextualGenRequest (Description d :> api) st where
--   genContextualRequest _ = genContextualRequest (Proxy :: Proxy api)

-- instance (HasContextualGenRequest b st, ToHttpApiData c )
--     => HasContextualGenRequest (Capture' mods x c :> b) st where
--     genContextualRequest _ = (oldf, do
--       old' <- old
--       new' <- toUrlPiece <$> _generator
--       return $ \burl st -> let r = old' burl st in r { path = cs new' <> path r })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)

-- instance (HasContextualGenRequest b st, ToHttpApiData c )
--     => HasContextualGenRequest (CaptureAll x c :> b) st where
--     genContextualRequest _ = (oldf, do
--       old' <- old
--       new' <- fmap (cs . toUrlPiece) <$> new
--       let new'' = BS.intercalate "/" new'
--       return $ \burl st -> let r = old' burl st
--         in r { path = new'' <> path r })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         new = _arbitrary  :: Gen [c]

-- instance (KnownSymbol h, HasContextualGenRequest b st, ToHttpApiData c)
--     => HasContextualGenRequest (Header' mods h c :> b) st where
--     genContextualRequest _ = (oldf, do
--       old' <- old
--       new' <- toUrlPiece <$> new  -- TODO: generate lenient or/and optional
--       return $ \burl st -> let r = old' burl st in r {
--           requestHeaders = (hdr, cs new') : requestHeaders r })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         hdr = fromString $ symbolVal (Proxy :: Proxy h)
--         new = _arbitrary :: Gen c

-- instance (AllMimeRender x c, HasContextualGenRequest b st)
--     => HasContextualGenRequest (ReqBody' mods x c :> b) st where
--     genContextualRequest _ = (oldf, do
--       old' <- old  -- TODO: generate lenient
--       new' <- new
--       (ct, bd) <- elements $ allMimeRender (Proxy :: Proxy x) new'
--       return $ \burl st -> let r = old' burl st in r {
--           requestBody = RequestBodyLBS bd
--         , requestHeaders = ("Content-Type", renderHeader ct) : requestHeaders r
--         })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         new = _arbitrary :: Gen c

-- instance (KnownSymbol x, ToHttpApiData c, HasContextualGenRequest b st)
--     => HasContextualGenRequest (QueryParam' mods x c :> b) st where
--     genContextualRequest _ = (oldf, do
--       new' <- new  -- TODO: generate lenient or/and optional
--       old' <- old
--       return $ \burl st -> let
--         r = old' burl st
--         newExpr = param <> "=" <> cs (toQueryParam new')
--         qs = queryString r in r {
--           queryString = if BS.null qs then newExpr else newExpr <> "&" <> qs })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         param = cs $ symbolVal (Proxy :: Proxy x)
--         new = _arbitrary :: Gen c

-- instance (KnownSymbol x, ToHttpApiData c, HasContextualGenRequest b st)
--     => HasContextualGenRequest (QueryParams x c :> b) st where
--     genContextualRequest _ = (oldf, do
--       (new' :: c) <- _fetch
--       old' <- old
--       return $ \burl st -> let r = old' burl st in r {
--           queryString = queryString r
--                      <> if length new' > 0 then fold (toParam <$> new') else ""})
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         param = cs $ symbolVal (Proxy :: Proxy x)
--         toParam c = param <> "[]=" <> cs (toQueryParam c)
--         fold = foldr1 (\a b -> a <> "&" <> b)

-- instance (KnownSymbol x, HasContextualGenRequest b st)
--     => HasContextualGenRequest (QueryFlag x :> b) st where
--     genContextualRequest _ = (oldf, do
--       old' <- old
--       return $ \burl st -> let r = old' burl st
--                                qs = queryString r in r {
--           queryString = if BS.null qs then param else param <> "&" <> qs })
--       where
--         (oldf, old) = genContextualRequest (Proxy :: Proxy b)
--         param = cs $ symbolVal (Proxy :: Proxy x)

-- instance (ReflectMethod method)
--     => HasContextualGenRequest (Verb (method :: k) (status :: Nat) (cts :: [*]) a) st where
--     genContextualRequest _ = (1, return $ \burl st -> defaultRequest
--        { host = cs $ baseUrlHost burl
--        , port = baseUrlPort burl
--        , secure = baseUrlScheme burl == Https
--        , method = reflectMethod (Proxy :: Proxy method)
--        })

-- instance (ReflectMethod method)
--     => HasContextualGenRequest (NoContentVerb (method :: k)) st where
--     genContextualRequest _ = (1, return $ \burl st -> defaultRequest
--        { host = cs $ baseUrlHost burl
--        , port = baseUrlPort burl
--        , secure = baseUrlScheme burl == Https
--        , method = reflectMethod (Proxy :: Proxy method)
--        })

-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (RemoteHost :> a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)

-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (IsSecure :> a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)

-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (HttpVersion :> a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)

-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (Vault :> a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)

-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (WithNamedContext x y a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)

-- -- TODO: Try logging in
-- instance (HasContextualGenRequest a st) => HasContextualGenRequest (BasicAuth x y :> a) st where
--     genContextualRequest _ = genContextualRequest (Proxy :: Proxy a)
