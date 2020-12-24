# roboservant

Automatically fuzz your servant apis in a contextually-aware way.

![CI](https://github.com/mwotton/roboservant/workflows/CI/badge.svg)

## example

see full example [here](EXAMPLE.md)

## why?

Servant gives us a lot of information about what a server can do. We
use this information to generate arbitrarily long request/response
sessions and verify properties that should hold over them.


## why not servant-quickcheck?

[servant-quickcheck](https://hackage.haskell.org/package/servant-quickcheck)
is a great package and I've learned a lot from it. Unfortunately, as mentioned previously,
there's a lot of the state space you just can't explore without context: modern webapps are
full of pointer-like structures, whether they're URLs or database
keys/uuids, and servant-quickcheck requires that you be able to generate
these without context via Arbitrary.
