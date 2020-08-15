{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
module Roboservant.StateMachine where

import           Roboservant.ContextualGenRequest
import           Servant.Server                   (Server)
import           Test.QuickCheck                  (Property)
import           Test.StateMachine


-- | can then use either `forAllParallelCommands` or `forAllCommands` to turn
--   this into a property
--
-- TODO: how do we tie the model, command and response types to the Server?
genStateMachine :: HasContextualGenRequest a => Server a -> StateMachine model command IO response
genStateMachine = undefined
