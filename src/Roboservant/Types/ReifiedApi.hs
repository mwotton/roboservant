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

type ReifiedApi = [(ApiOffset, ReifiedEndpoint )]

tagType :: Typeable a => f a -> TypedF f a
tagType = (R.typeRep :*:)

newtype InteractionError = InteractionError T.Text
  deriving Show
instance Exception InteractionError
