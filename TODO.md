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
