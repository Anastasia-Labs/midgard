# 0003 — Publishable semantic resolvers

- Status: Accepted; validation and transition resolver decomposition implemented
- Scope: physical validator decomposition under the L1 transaction envelope
- Recorded: 2026-09-07, extracted from delivered plans and checked against source;
  this date is not a new protocol acceptance or measurement receipt.

## Context

The former validation and transition monoliths exceeded signed publication or
aggregate execution limits. Narrow decoding alone could not make every semantic
arm fit. Repeating large witness decoding in several rewarding validators also
exhausted the per-transaction budget.

## Decision

First prune unreachable semantic branches without changing the predicate. When
an arm still does not fit, use authenticated zero-withdrawal yields if the
aggregate transaction fits, or checkpointed computation-thread continuations
when work must span transactions. Measure fully applied, signed publications and
all executing scripts together; raw body size is only a diagnostic.

A dispatcher authenticates each required rewarding script through the exact
reference role NFT and zero withdrawal. A yield finds exactly one eligible
dispatcher input and its spend redeemer, verifies its assigned predicate, and
binds the exact successor or claimed result. All required roles form a
conjunction: omitting, duplicating, or substituting a role cannot discharge an
arm. Shared yields must authenticate which eligible dispatcher they serve.

Forward-only chains authenticate immutable origin, stage/progress state, and
exact forward output script hashes. Avoid cyclic applied hash dependencies;
a return route must be fixed or frozen in authenticated continuation state.
Every unfinished physical step retains explicit cancellation.

CEK context stages that traverse redeemer items reuse the ScriptSources item
continuation with a carrier binding the pending CEK state, item control, witness
hash, and claimed successor. Execution-selection material traversal authenticates
one typed task at a time, including already visited nodes' type and length.
Only an empty pending stack with exact node/byte totals can reach the award.
Program, Data and blob traversal must not admit unreachable entries.

Transition accepted-transaction and deposit finalizers share authenticated output
summary verification, while their dispatchers retain source-specific binding and
terminal checks. ValueAndMint resolvers share asset-fold semantics while retaining
the exact replay/output/mint arm boundary. Canonical descriptor asset frontiers
use native length-then-bytes key order; evaluated script contexts use
lexicographic asset-name order. Mixed-width names require an authenticated
permutation that preserves quantities and the complete asset set, not equality
of those two orderings.

## Alternatives and consequences

Increasing emulator limits, disabling evaluation, or publishing an oversized
reference script is not acceptance. A blanket all-yields transaction was rejected
where its aggregate memory exceeded the limit; adding transaction boundaries
preserves the predicate but increases fees, reference-script costs, and elapsed
challenge time. Those costs require measured lifecycle evidence.

Publication tests enforce a 512-byte reserve under the 16,384-byte signed envelope.
Execution uses the shared Van Rossem limits and the applicable fit-ledger reserve.
Rebuild and remeasure after any change to compiler, applied parameters, shared
libraries, witness frontier, or protocol limits.

## Authorities

- [Semantic yield handshake](../../../onchain/aiken/lib/midgard/validation-semantic-yield-v1.ak)
- [Transition final yields](../../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/final-yield.ak)
- [CEK context chain](../../../onchain/aiken/lib/midgard/cek-context-chain.ak)
- [Installed validation dispute](../validation-trace-dispute-installed-workflow.md)
- [Installed transition replay](../transition-trace-installed-replay.md)
- [Size evidence and active availability work](../size-plans/README.md)
