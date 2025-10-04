# Example

Our api under test:

``` haskell
-- Obligatory fancy-types pragma tax
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Roboservant.Server as RS
import qualified Roboservant.Client as RC
import Servant.Client(ClientEnv, baseUrlPort, parseBaseUrl, mkClientEnv)
import Network.HTTP.Client       (defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import Roboservant.Types
import Test.Syd
import Servant
import Servant.API.Generic 
import Servant.Server.Generic (AsServer)
import GHC.Generics
import Data.Typeable
import Data.Hashable
import Data.Maybe(isNothing, isJust)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Aeson(FromJSON,ToJSON)

newtype A = A Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (Hashable, FromHttpApiData, ToHttpApiData)

instance FromJSON A
instance ToJSON A

newtype B = B Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (Hashable, FromHttpApiData, ToHttpApiData)

instance FromJSON B
instance ToJSON B

type Api =
  "item" :> Get '[JSON] A
    :<|> "itemAdd" :> Capture "one" B :> Capture "two" B :> Get '[JSON] B
    :<|> "item" :> Capture "itemId" B :> Get '[JSON] ()

server :: Handler A -> Server Api
server introduce = introduce :<|> combine :<|> eliminate
  where
    combine (B i) (B j) = pure $ B (i + j)
    eliminate (B i)
      | i > 10 = error "give up, eleven is way too big and probably not even real"
      | otherwise = pure ()
```

We have a "good" server, that never generates anything other than a 0. This means repeated application of
the combination/addition rule can never bring us to the dangerous state of numbers larger than 10.

``` haskell
goodServer, badServer :: Server Api
goodServer = server (pure $ A 0)
badServer = server (pure $ A 1)
```

In the test file, we first define the tests: the faulty server should fail and the good server should pass.

```haskell
main :: IO ()
main = sydTest spec

spec :: Spec
spec = describe "example" $ do
  it "good server should not fail" $ do
    RS.fuzz @Api goodServer config
      >>= (`shouldSatisfy` isNothing)
  it "bad server should fail" $ do
    RS.fuzz @Api badServer config
      >>= (`shouldSatisfy` isJust)
```

The previous test just picked apart the server and ran functions manually: sometimes, we want to test via
an honest-to-goodness network port, like so:

```haskell
  around (withServer (serve (Proxy :: Proxy Api) badServer)) $ do
    it "we should also be able to run the _client_ to an independent server (ignore server error messages)" $ \(clientEnv::ClientEnv) -> do
      RC.fuzz @Api clientEnv config >>= (`shouldSatisfy` isJust)
```

(we use withApplication rather than testWithApplication because we don't primarily care what the server does here:
we want to check what a client does when presented with a faulty server.)

We expect to be able to cover the whole api from our starting point, so let's set the coverage to 0.99.
There are other tweakable things in the config, like maximum runtime, reps,
per-request healthchecks, seeds, and verbose logging. Have a look at
Roboservant.Types.Config for details.

``` haskell
config :: Config
config = defaultConfig
  { coverageThreshold = 0.99
  }
```

Unless we want to ship roboservant and all its dependencies to production, we also need
some orphan instances: because As are the only value we can get without
an input, we need to be able to break them down.

``` haskell
deriving via (Compound A) instance Breakdown A
```

if we wanted to assemble As from parts as well, we'd derive using Compound, but in this case we don't care.

``` haskell
deriving via (Atom A) instance BuildFrom A

```

Similarly, to generate the first B from the Ints we got from inside the A, we need to be able to
build it up from components.

```haskell
deriving via (Compound B) instance BuildFrom B
deriving via (Atom B) instance Breakdown B
```


## Record APIs

Roboservant can drive record-style servant APIs that use
`Servant.API.Generic` and `Servant.Server.Generic`. Define the
record-of-routes, provide a record server, and `fuzz` works as it does
for vanilla route trees.

```haskell
data RecordRoutes mode = RecordRoutes
  { recordGet :: mode :- Get '[JSON] Int
  , recordDelete :: mode :- Capture "unused" Int :> Delete '[JSON] ()
  }
  deriving stock (Generic)

type RecordApi = NamedRoutes RecordRoutes

recordServer :: RecordRoutes AsServer
recordServer =
  RecordRoutes
    { recordGet = pure 42
    , recordDelete = const (pure ())
    }

recordSpec :: Spec
recordSpec =
  describe "record apis" $
    it "fuzzes a NamedRoutes server" $
      RS.fuzz @RecordApi recordServer config
        >>= (`shouldSatisfy` isNothing)
```


test utilities:

``` haskell
withServer :: Application -> ((ClientEnv -> IO ()) -> IO ())
withServer app action = Warp.withApplication (pure app) (\p -> genClientEnv p >>= action)
  where genClientEnv port = do
          baseUrl <- parseBaseUrl "http://localhost"
          manager <- newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 1000000 }
          pure $ mkClientEnv manager (baseUrl { baseUrlPort = port })
```
