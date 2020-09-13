# roboservant

Automatically fuzz your servant apis in a contextually-aware way.

![CI](https://github.com/mwotton/roboservant/workflows/CI/badge.svg)


# why?

Servant gives us a lot of information about what a server can do. We
use this information to generate arbitrarily long request/response
sessions and verify properties that should hold over them.

# example

Our api under test:

```
newtype Foo = Foo Int
  deriving (Generic, Eq, Show, Typeable)
  deriving newtype (FromHttpApiData, ToHttpApiData)

type FooApi =
  "item" :> Get '[JSON] Foo
    :<|> "itemAdd" :> Capture "one" Foo :> Capture "two" Foo :> Get '[JSON] Foo
    :<|> "item" :> Capture "itemId" Foo :> Get '[JSON] ()
```

From the tests:

```
  let reifiedApi = RS.toReifiedApi (RS.flattenServer @Foo.FooApi Foo.fooServer) (Proxy @(Endpoints Foo.FooApi))
  assert "should find an error in Foo" . not
    =<< checkSequential (Group "Foo" [("Foo", RS.prop_sequential reifiedApi)])
```

We have a server that blows up if the value of the int in a `Foo` ever gets above 10. Note:
there is no generator for `Foo` types: larger `Foo`s can only be made only by combining existing
`Foo`s with `itemAdd`. This is an important distinction, because many APIs will return UUIDs or
similar as keys, which make it impossible to cover a useful section of the state space without
using the values returned by the API


# why not servant-quickcheck?

[servant-quickcheck](https://hackage.haskell.org/package/servant-quickcheck)
is a great package and I've learned a lot from it. Unfortunately, as mentioned previously,
there's a lot of the state space you just can't explore without context: modern webapps are
full of pointer-like structures, whether they're URLs or database
keys/uuids, and servant-quickcheck requires that you be able to generate
these without context via Arbitrary.

## extensions/todo

- add some "starter" values to the store
  - there may be a JWT that's established outside the servant app, for instance.
- `class Extras a where extras :: Gen [a]`
  - default implementation `pure []`
  - selectively allow some types to create values we haven't seen from the api.
	`newtype FirstName = FirstName Text`, say.
- break down each response type into its components
  - if i have
	- `data Foo = FBar Bar | FBaz Baz`
	- an endpoint `foo` that returns a `Foo`
	- and an endpoint `bar` that takes a `Bar`
  - I should be able to call `foo` to get a `Foo`, and if it happens to be an `FBar Bar`, I
	should be able to use that `Bar` to call `bar`.
- better handling of properties to be verified
  - some properties should always hold (no 500s): this already works.
  - to-do: there may be some other properties that hold contextually
	- healthcheck should be 200
	- test complex permissions/ownership/delegation logic - should never be able to
	  get access to something you don't own or haven't been delegated access to.

## other possible applications

- coverage
  - if you run the checker for a while and `hpc` suggests you still have bad coverage,
	your api is designed in a way that requires external manipulation and may be improvable.

- benchmarking
  - we can generate "big-enough" call sequences, then save the database & a sample call for each
	endpoint that takes long enough to be a reasonable test.
  - from this we can generate tests that a given call on that setup never gets slower.
