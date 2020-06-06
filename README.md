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
