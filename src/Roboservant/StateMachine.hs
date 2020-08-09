{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
module Roboservant.StateMachine where

import           Servant.Server    (Server)
import           Test.QuickCheck   (Property)
import           Test.StateMachine
import Roboservant.ContextualGenRequest


-- | can then use either `forAllParallelCommands` or `forAllCommands` to turn
--   this into a property
--
-- TODO: how do we tie the model, command and response types to the Server?
genStateMachine :: HasContextualGenRequest a b => Server a -> StateMachine model command IO response
genStateMachine = undefined
