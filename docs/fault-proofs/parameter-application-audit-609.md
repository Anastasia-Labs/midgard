# Blueprint parameter-application contract

This file is the standing parameter-application reference for source and
evidence links.

## Current rule

Every deployment builder must apply exactly the parameters declared by the
selected blueprint validator, in declared order. A missing `parameters` field
means zero declared parameters. Supplying too few, too many, or an unresolved
named parameter fails closed before a transaction is built.

The two production enforcement points are:

- `applyBlueprintParams` in
  `demo/midgard-sdk/src/fraud-proof/contracts.ts`; and
- `applyBlueprintDeclaredParams` in
  `demo/midgard-node/src/services/midgard-contracts.ts`.

Bare-script helpers may return only a validator that declares zero parameters.
All applied script hashes and addresses must be derived after this gate.

## Semantic resolver application

Validation semantic resolvers are applied from their blueprint-declared
parameter names. The resolver table supplies values for known names; an unknown
name is an error. Count-only or index-specific hand-written argument lists are
not acceptable because resolver parameter sets differ.

`demo/midgard-fault-proofs/tests/semantic-resolver-arity-gate.test.ts` checks
that deployed resolvers equal their full declared application, differ from
under-applied prefixes, and that production builders cannot bypass the guarded
application helpers.

## Current deployment identity

The generated testnet blueprint currently contains 563 validators and has
SHA-256
`b885c3abb0eeaace296011a108fbe4a06d0e5303bfb9d73bbec48fc30f32f9de`.
The 32-category catalogue derives root
`85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`.
The inspection suite pins and verifies that root. These are
whole-artifact identities, not constants to copy into application logic. Any
validator, compiler, parameter, or catalogue change requires
regeneration and re-binding.

## Verification result

The repository-wide guarded-application scan and semantic-resolver arity suite
cover the current production application paths; zero additional under-applied
validators were found. Future changes must rerun those checks rather than
relying on this sentence as durable evidence.
