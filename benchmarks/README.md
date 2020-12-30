# Benchmarks

I've noticed compilation can be a little slow on realistic apps so I set up a test.

## components

1. codegen

    given an integer input n, create an api with n endpoints. they should take a reasonably
    complicated input and return a reasonably complicated output

2. runner

    build dependencies
    for i from 1 to 100
        generate api with i endpoints
        compile it, but timed
        print (i,time)

3. plot to see linearity/quadraticity?

## possible gotchas

- we usually compile these in different modules rather than together: if we don't get slow
  compilation in one file, try out multiple files.

## notes

- flat profile if you don't have the call to `fuzz`
- no difference between deriving via Atom vs Compound, so not a Generics thing.
