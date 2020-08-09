# roboservant

Automatically fuzz your servant apis in a contextually-aware way.

![CI](https://github.com/mwotton/roboservant/workflows/CI/badge.svg)

# why?

Servant gives us a lot of information about what a server can do. We
use this information to generate arbitrarily long request/response
sessions and verify properties that should hold over them.

# why not servant-quickcheck?

[servant-quickcheck](https://hackage.haskell.org/package/servant-quickcheck)
is a great package and I've learned a lot from it. Unfortunately,
there's a lot of the state space it can't explore: modern webapps are
full of pointer-like structures, whether they're URLs or database
keys/uuids. servant-quickcheck demands that you be able to generate
these without context via Arbitrary: good luck exploring an API that
requires you to generate just the right UUID to hit non-trivial code.

roboservant avoids this by using
[quickcheck-state-machine](https://github.com/advancedtelematic/quickcheck-state-machine#readme),
which models the dynamic state in such a way that we can use results
of previous calls.

# concept

we start with a servant api and a server that fulfills the type.

From that api, we should be able to summon up an empty type-indexed store with a key
for each response type in the API.

We can then look at each callable endpoint, and eliminate any that require values that are
empty in the type-indexed store. this allows a generative process where we can extend a sequence
of calls indefinitely, by making a call to the concrete server and recording the result in the
type-indexed store.

## extensions
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

## applications
- testing
  - some properties should always hold (no 500s)
  - there may be some other properties that hold contextually
	- healthcheck should be 200
	- test complex permissions/ownership/delegation logic - should never be able to
	  get access to something you don't own or haven't been delegated access to.

- benchmarking
  - we can generate "big-enough" call sequences, then save the database & a sample call for each
	endpoint that takes long enough to be a reasonable test.
  - from this we can generate tests that a given call on that setup never gets slower.
