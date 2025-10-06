module Roboservant.Types.Config where

import Data.Dynamic
import Data.List.NonEmpty (NonEmpty)
import Roboservant.Types.Internal (Provenance)
import Roboservant.Types.ReifiedApi (ApiOffset, InteractionError)

data Config
  = Config
      { seed :: [(Dynamic, Int)],
        maxRuntime :: Double, -- seconds to test for
        maxReps :: Integer,
        rngSeed :: Int,
        coverageThreshold :: Double,
        logInfo :: String -> IO (),
        healthCheck :: IO (),
        traceChecks :: [TraceCheck]
      }

data TraceResult
  = TraceSuccess (NonEmpty Dynamic)
  | TraceError InteractionError
  deriving (Show)

data CallTrace
  = CallTrace
      { ctOffset :: ApiOffset,
        ctProvenance :: [Provenance],
        ctArguments :: [Dynamic],
        ctResult :: TraceResult
      }
  deriving (Show)

data TraceCheck
  = TraceCheck
      { traceCheckName :: String,
        traceCheck :: [CallTrace] -> Maybe String
      }

defaultConfig :: Config
defaultConfig =
  Config
    { seed = [],
      maxRuntime = 0.5,
      maxReps = 1000,
      rngSeed = 0,
      coverageThreshold = 0,
      logInfo = const (pure ()),
      healthCheck = pure (),
      traceChecks = []
    }

noisyConfig :: Config
noisyConfig = defaultConfig {logInfo = print}
