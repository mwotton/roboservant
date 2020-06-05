

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

import GHC.TypeLits
import Control.Applicative
import Servant.API

newtype Foo = Foo Int
  deriving Show

newtype Bar = Bar String
  deriving Show

type FooApi = "foo" :> "fle" :> "far" :> Get '[JSON] Foo

type BarApi = "bar" :>  ReqBody '[JSON] Foo :> Post '[JSON] Bar

type Api = FooApi
      :<|> BarApi

type family ExtractRespType (path :: *) :: * where
  ExtractRespType (_ :> b) = ExtractRespType b
  ExtractRespType (Verb (method :: StdMethod) (responseCode :: Nat) (contentTypes :: [*]) (respType :: *)) = respType

type Foo' = ExtractRespType FooApi


test :: ()
test = foo'EqualFoo where
  foo'EqualFoo :: Foo' ~ Foo => ()
  foo'EqualFoo = ()


test' :: ()
test' = blah where
  blah :: ExtractRespTypes Api ~ '[Foo, Bar] => ()
  blah = ()

type family ExtractRespTypes (paths :: *) :: [*] where
  ExtractRespTypes (a :<|> b) = ExtractRespTypes a <> ExtractRespTypes b
  ExtractRespTypes a = '[ExtractRespType a]

type family (<>) (xs :: [k]) (ys :: [k]) :: [k] where
  (x ': xs) <> ys = x ': (xs <> ys)
  '[] <> ys = ys

data List :: (* -> *) -> [*] -> * where
  Cons :: f a -> List f as -> List f (a ': as)
  Nil :: List f '[]

class Insert f a as where
  insert :: a -> List f as -> List f as

class Lookup f a as where
  lookup :: List f as -> f a

instance Show (List f '[]) where
  show Nil = "Nil"

instance (Show (List f as), forall a. Show a => Show (f a), Show a) => Show (List f (a ': as)) where
  show (Cons fa fas) = "(Cons " <> show fa <> " " <> show fas <> ")"

storeOfApi :: forall api xs. (ExtractRespTypes api ~ xs, BuildStore [] xs) => List [] (ExtractRespTypes api)
storeOfApi = buildStore

storeOfOurApi = storeOfApi @Api

class BuildStore f (xs :: [*]) where
  buildStore :: List f xs

instance BuildStore f '[] where
  buildStore = Nil

instance (BuildStore f xs, Alternative f) => BuildStore f (x ': xs) where
  buildStore = Cons empty (buildStore @_ @xs)

main :: IO ()
main = pure ()

-- 1.
-- Instead of just the resptype, let's return a tuple of (resptype, '[]::[ ? respType])

-- 2.
-- break down respType into "useful" components
--   data Baz = Baz { key1 :: Key Foo, key2 :: Key Bar }
--   should insert into type-indexed list three elements
--     'Baz -> whole value
--     '(Key Bar) -> key2
--     '(Key Foo) -> key1

