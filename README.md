# roboservant

Automatically fuzz your servant apis in a contextually-aware way.

[![Stack CI](https://github.com/mwotton/roboservant/actions/workflows/ci.yml/badge.svg)](https://github.com/mwotton/roboservant/actions/workflows/ci.yml)
[![Cabal CI](https://github.com/mwotton/roboservant/actions/workflows/cabal.yml/badge.svg)](https://github.com/mwotton/roboservant/actions/workflows/cabal.yml)

## example

see full example [here](EXAMPLE.md)

Record-style servant APIs built with `Servant.API.Generic` and
`NamedRoutes` are supported as well; the example file now includes a
walkthrough of fuzzing a record server.

## why?

Servant gives us a lot of information about what a server can do. We
use this information to generate arbitrarily long request/response
sessions and verify properties that should hold over them.

## how?

In essence, ```fuzz @Api yourServer config``` will make a bunch of
calls to your API, and record the results in a type-indexed
dictionary. This means that they are now available for the
prerequisites of other calls, so as you proceed, more and more api
calls become possible.

### fuzzing with sydtest

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import qualified Roboservant as R
import qualified Roboservant.Server as RS
import Servant (Get, JSON, Proxy (..), (:>), Server)
import Test.Syd

type Api = "ping" :> Get '[JSON] Int

server :: Server Api
server = pure 42

main :: IO ()
main = sydTest $ do
  describe "ping api" $
    it "stays healthy under fuzzing" $ do
      RS.fuzz @Api server R.defaultConfig {R.maxReps = 200}
        >>= (`shouldBe` Nothing)
```

### fuzzing remote servers

You don't have to embed the server under test in-process. If you have
an instance running elsewhere that implements the same Servant API, you
can point Roboservant at its base URL and let the fuzzer drive the
endpoints:

```haskell
import qualified Roboservant.Client as RC
import qualified Roboservant as R
import Servant.Client (parseBaseUrl)
import Test.Syd

remoteSpec :: Spec
remoteSpec =
  it "accepts the happy-path flow" $ do
    base <- either (fail . show) pure (parseBaseUrl "http://localhost:8080")
    RC.fuzzBaseUrl @Api base R.defaultConfig >>= (`shouldBe` Nothing)

main :: IO ()
main = sydTest remoteSpec
```

For quick scripts you can also pass the URL as a string and let
Roboservant parse it for you with `fuzzUrl`.

### checking trace-level invariants

Roboservant can evaluate predicates over the entire sequence of calls
by attaching `TraceCheck`s to the configuration. Use this to catch
subtle behaviours (for example, a 401 emitted before authentication is
complete):

```haskell
import Data.List (find)
import qualified Data.Text as T
import qualified Roboservant as R
import qualified Roboservant.Server as RS

noUnauthorized :: [R.CallTrace] -> Maybe String
noUnauthorized calls =
  case find isUnauthorized calls of
    Nothing -> Nothing
    Just _ -> Just "encountered 401 before authorization"
  where
    isUnauthorized R.CallTrace {R.ctResult = R.TraceError err} =
      (not . R.fatalError) err && "401" `T.isInfixOf` R.errorMessage err
    isUnauthorized _ = False

let config =
      R.defaultConfig
        { R.traceChecks =
            [ R.TraceCheck
                { R.traceCheckName = "no unauthorized",
                  R.traceCheck = pure . noUnauthorized
                }
            ]
        }

RS.fuzz @Api server config >>= (`shouldBe` Nothing)
```

`TraceCheck` actions run in `IO`, so they can query databases,
metrics, or any other state the server closes over. Simply return
`Nothing` when the invariant holds or `Just failure` (for any
`Show`able type) when it does not.

### reading failure reports

When a fuzz run fails, Roboservant prints the minimized HTTP trace that
triggered the issue and stores it in `.minithesis-db` for later
inspection. A failure now renders each call with its method, URL
segments, query parameters, headers, and payloads. For example:

```
POST /checkout?id=42 -> ok
  body: {"item":"widget","quantity":3}
  response: {"orderId":"8b9f6e"}
GET /fail/777 -> ERROR explosion (fatal)
```

The same trace is persisted in `.minithesis-db`, so you can replay or extend the
reproducer later.

We explicitly do not try to come up with plausible values that haven't
somehow come back from the API. That's straying into QC/Hedgehog
territory: if you want that, come up with the values on that side, and
set them as seeds in the configuration.

### what does it mean to be "available"?

In a simple API, you may make a call and get back a `Foo`, which will
allow you to make another call that requires a `Foo`. In a more
complicated app, it's likely that you'll send a request body that
includes many subcomponents, and it's likely you'll get a response
that needs to be broken down into pieces before it's useful.

To cope with this, we have the typeclasses `BuildFrom` and
`Breakdown`. You can write instances for them if you feel like it, and
indeed it's currently required for recursive datatypes if you don't
want the fuzzer to hang, but for the majority of your types it should
be sufficient to derive them generically. (Sensible instances are
provided for lists.)

There are two basic strategies here. In some cases, you want to regard
a type as indivisible: that's why we like newtypes, right? In this
case, we can derive using the `Atom` strategy.

``` haskell
deriving via (Atom NewtypedKey) instance Breakdown NewtypedKey
deriving via (Atom NewtypedKey) instance BuildFrom NewtypedKey
```

This is saying "A can neither be built from components or broken down
for spare parts. Hands off!". This is a good strategy for key types,
for instance.

If instead it's a big complicated thing with lots of juicy
subcomponents, we want to rip it apart using Generics and feast on
its succulent headmeats:

``` haskell
deriving via (Compound Payload) instance Breakdown Payload
deriving via (Compound Payload) instance BuildFrom Payload
```

### priming the pump

Sometimes there are values we'd like to smuggle into the API that are
not derivable from within the API itself: sometimes this is a warning
sign that your API is incomplete, but it can be quite reasonable to
require identifying credentials within an API and not provide a way to
get them. It might also be reasonable to have some sample values that
the user is expected to come up with.

For those cases, override the `seed` in the `Config` with a
list of seed values, suitably hashed:

``` haskell
defaultConfig { seed = [hashedDyn creds, hashedDyn userJwt]}
```

## why not servant-quickcheck?

[servant-quickcheck](https://hackage.haskell.org/package/servant-quickcheck)
is a great package and I've learned a lot from it. Unfortunately, as mentioned previously,
there's a lot of the state space you just can't explore without context: modern webapps are
full of pointer-like structures, whether they're URLs or database
keys/uuids, and servant-quickcheck requires that you be able to generate
these without context via Arbitrary.

## limitations and future work

Failure traces now contain the exact operations that ran (including
URLs, query parameters, headers, bodies, and responses) and can be
checked with `TraceCheck`s. Minithesis shrinks and persists the
smallest failing sequence in `.minithesis-db` for later inspection.

Support for recursive datatypes still requires hand-written
`BuildFrom` instances to avoid infinite loops. Deriving those
automatically (or rejecting problematic definitions earlier) remains
on the roadmap.

Finally, the `FlattenServer` instance for `:>` is still quadratic. A
more efficient representation would make large APIs cheaper to fuzz.
