{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Roboservant where

import Prelude hiding (lookup)
import GHC.TypeLits
import Servant.API

-- | Extract the response types from a single servant endpoint, i.e. one
-- without any ':<|>' type constructors in it.
type family ExtractRespType (path :: *) :: * where
  ExtractRespType (_ :> b) = ExtractRespType b
  ExtractRespType (Verb (method :: StdMethod) (responseCode :: Nat) (contentTypes :: [*]) (respType :: *)) = respType

-- | Extract the response types from a flattened servant API, i.e. one
-- which has the distributive law (modulo isomorphism) applied to it until
-- it reaches a normal form.
type family ExtractRespTypes (paths :: *) :: [*] where
  ExtractRespTypes (a :<|> b) = ExtractRespTypes a <> ExtractRespTypes b
  ExtractRespTypes a = '[ExtractRespType a]

-- | Append two type level lists.
type family (<>) (xs :: [k]) (ys :: [k]) :: [k] where
  (x ': xs) <> ys = x ': (xs <> ys)
  '[] <> ys = ys

-- | A homogeneous-functor-list, I guess.
data List :: (* -> *) -> [*] -> * where
  Cons :: f a -> List f as -> List f (a ': as)
  Nil :: List f '[]

-- | So we can use efficient container types for specific types if the need
-- arises.
class Listy f a where
  cons :: a -> f a -> f a
  empty :: f a
  uncons :: f a -> Maybe (a, f a)

instance Listy [] a where
  cons = (:)
  empty = []
  uncons = \case
    a : as -> Just (a, as)
    [] -> Nothing

-- | A function for inserting elements into their slot in the store.
class Insert f a as where
  insert :: a -> List f as -> List f as
instance {-# OVERLAPPABLE #-} Listy f a => Insert f a (a ': as) where
  insert a (Cons as ls) = Cons (cons a as) ls
instance {-# OVERLAPPABLE #-} Insert f a as => Insert f a (b ': as) where
  insert a (Cons as ls) = Cons as (insert a ls)

-- | A function for looking up the container of elements for a specific
-- type in the store.
class Lookup f a as where
  lookup :: List f as -> f a

instance {-# OVERLAPPABLE #-} Lookup f a (a ': as) where
  lookup (Cons fa ls) = fa
instance {-# OVERLAPPABLE #-} Lookup f a as => Lookup f a (b ': as) where
  lookup (Cons fa ls) = lookup ls

deriving instance (Show (List f as), forall a. Show a => Show (f a), Show a)
  => Show (List f (a ': as))

instance Show (List f '[]) where show _ = "Nil" 

storeOfApi :: BuildStore [] (ExtractRespTypes api) => List [] (ExtractRespTypes api)
storeOfApi = buildStore

class BuildStore f xs where
  buildStore :: List f xs

instance BuildStore f '[] where
  buildStore = Nil

instance (BuildStore f as, Listy f a) => BuildStore f (a ': as) where
  buildStore = Cons empty buildStore
-- 1.
-- Instead of just the resptype, let's return a tuple of (resptype, '[]::[ ? respType])

-- 2.
-- break down respType into "useful" components
--   data Baz = Baz { key1 :: Key Foo, key2 :: Key Bar }
--   should insert into type-indexed list three elements
--     'Baz -> whole value
--     '(Key Bar) -> key2
--     '(Key Foo) -> key1
--
