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

`demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts` checks
that deployed resolvers equal their full declared application, differ from
under-applied prefixes, and that production builders cannot bypass the guarded
application helpers.

## Current deployment identity

The generated testnet blueprint currently contains 510 validators and has
SHA-256
`ad69e8f98e49e110864cb270dd6bb731caaf43357e8459827b1659124c890de8`.
The 29-category catalogue derives root
`c686373893084eff5efe51a52821055f994caa4c26a363df37ec97df23380b62`.
The inspection suite's static expected root is still stale. These are
whole-artifact identities, not constants to copy into application logic. Any
validator, compiler, parameter, or catalogue change requires
regeneration and re-binding.

## Verification result

The repository-wide guarded-application scan and semantic-resolver arity suite
cover the current production application paths; zero additional under-applied
validators were found. Future changes must rerun those checks rather than
relying on this sentence as durable evidence.
