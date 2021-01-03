module Roboservant.Types.Config where

import Data.Dynamic

data Config
  = Config
      { seed :: [(Dynamic, Int)],
        maxRuntime :: Double, -- seconds to test for
        maxReps :: Integer,
        rngSeed :: Int,
        coverageThreshold :: Double,
        logInfo :: String -> IO (),
        healthCheck :: IO ()
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
      healthCheck = pure ()
    }

noisyConfig :: Config
noisyConfig = defaultConfig {logInfo = print}
