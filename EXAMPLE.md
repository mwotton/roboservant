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

import Roboservant
import Test.Hspec
import Servant
import GHC.Generics
import Data.Typeable
import Data.Hashable
import Data.Maybe(isNothing, isJust)

newtype A = A Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (Hashable, FromHttpApiData, ToHttpApiData)

newtype B = B Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (Hashable, FromHttpApiData, ToHttpApiData)

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
the combination/addition rule can never bring us to the dangerous state of numbers larger than 100.

``` haskell
goodServer, badServer :: Server Api
goodServer = server (pure $ A 0)
badServer = server (pure $ A 1)
```

In the test file, we first define the tests: the faulty server should fail and the good server should pass.

```haskell
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "example" $ do
  it "good server should not fail" $ do
    fuzz @Api goodServer config
      >>= (`shouldSatisfy` isNothing)
  it "bad server should fail" $ do
    fuzz @Api badServer config
      >>= (`shouldSatisfy` isJust)
```


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
