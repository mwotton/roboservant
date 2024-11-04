# roboservant

Automatically fuzz your servant apis in a contextually-aware way.

[![Stack CI](https://github.com/mwotton/roboservant/actions/workflows/ci.yml/badge.svg)](https://github.com/mwotton/roboservant/actions/workflows/ci.yml)
[![Cabal CI](https://github.com/mwotton/roboservant/actions/workflows/cabal.yml/badge.svg)](https://github.com/mwotton/roboservant/actions/workflows/cabal.yml)

##

This is pretty much obsoleted by [schemathesis](https://schemathesis.readthedocs.io/en/stable/), at least once you 
have stateful testing going.

## example

see full example [here](EXAMPLE.md)

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

Currently, the display of failing traces is pretty tragic, both in the
formatting and in its non-minimality. This is pretty ticklish:
arguably the right way to do this is to return a trace that we can
also rerun, and let quickcheck or hedgehog a level up shrink it until
it's satisfactorily short. In the interest of being useful earlier
rather than later, I'm releasing v1.0 before I crack this particular
nut. We do know which calls we made that led to the failing case, so
we would want to show that distinction in a visible way: it's possible
that other calls that don't have direct data dependencies were
important, but we definitely know we need the direct data dependencies.

The provenance stuff is a bit underbaked. It should at least pull a
representation of the route chosen rather than just an integer index.

It would also be nice to have a robust strategy for deriving recursive
datatypes, or at least rejecting attempts to generate them that don't
end in an infinite loop.

Currently the `FlattenServer` instance for `:>` is quadratic. It would
be nice to fix this but I lack the art.
