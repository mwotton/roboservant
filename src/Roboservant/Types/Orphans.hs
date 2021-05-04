{-# OPTIONS_GHC -fno-warn-orphans #-}
module Roboservant.Types.Orphans where

import Servant
import Data.Hashable

instance Hashable NoContent
