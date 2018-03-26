# monad-control-aligned

An alternative edition of monad-control that forces all state data types to:

- be exposed at the `Constraint` level
- be aligned s.t. the state is of kind `* -> *`, wherein the subject data type is indeed the monadic value `a` in `m a`;
  this opposes the `StateT` and `WriterT` definitions, whose state types are in the form of `(a, s)`; opposing curried
  type application indeed. Our edition enforces instances in the form of `(,) s` - thus leaving the last applied type polymorphic,
  achieving our goal of `* -> *` _aligned_ [tm] state type.

This just allows us to use extractable-singleton to _run_ our state type - useful for obtaining the subject data while
running in a lifted context - basically exactly the same as what monad-unlift does.
