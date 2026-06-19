# Task 02: Root Primitives And Proof Witnesses

## Goal

Make the authenticated data structure layer capable of proving all source,
trace, and exact-once conditions needed by transition trace fault proofs.

This task is complete when TypeScript can build and verify the same membership,
non-membership, count, and indexed-step witnesses that Aiken validators will
consume.

## Files To Update

TypeScript root utilities:

- `demo/midgard-node/src/workers/utils/mpf/phas.ts`
- `demo/midgard-node/src/workers/utils/mpf.ts`
- `demo/midgard-node/src/workers/commit-block-header/event-roots.ts`
- Add a new focused module if useful:
  `demo/midgard-node/src/workers/commit-block-header/transition-roots.ts`

SDK proof types:

- `demo/midgard-sdk/src/ledger-state.ts`
- `demo/midgard-sdk/src/common.ts`
- Add `demo/midgard-sdk/src/transition-trace.ts` if the ledger-state file would
  become too broad.

Aiken proof utilities:

- `onchain/aiken/lib/midgard/ledger-state.ak`
- Existing Merkle or PHAS proof libraries under `onchain/aiken/lib/`
- Existing fraud-proof helper modules under
  `onchain/aiken/lib/midgard/fraud-proofs/`

Tests:

- `demo/midgard-node/tests/mpf.test.ts`
- `demo/midgard-node/tests/cbor-root-normalization.test.ts`
- Add transition-root fixture tests under `demo/midgard-node/tests/`.

## Proof Capabilities Required

### Source Roots

For each source root:

```text
withdrawals_root
forced_transactions_root
transactions_root
deposits_root
```

the verifier needs:

- membership by source key
- non-membership by source key
- count/range proof or another authenticated way to bind committed counts
- deterministic root construction from sorted key/value pairs

### Transition Trace Root

The trace root is a dense vector-like map:

```text
step_index -> TransitionStep
```

The verifier needs:

- membership at `step_index`
- non-membership or range proof for out-of-range steps
- count binding to `transition_step_count`
- ability to prove two adjacent leaves for link faults

### Event To Step Root

The event-to-step root is an authenticated map:

```text
EventKey -> EventToStepValue
```

The verifier needs:

- membership by `EventKey`
- non-membership by `EventKey`
- ability to prove that a source event has no step
- ability to prove that a trace step's `event_key` maps to a different step

## Types To Add Or Change

Define proof witness types with explicit domain separation:

```text
RootMembershipProof<K, V>
RootNonMembershipProof<K>
RootCountProof
IndexedTraceProof
AdjacentTraceProof
EventToStepProof
```

If the existing PHAS proof type can already represent these, define aliases and
document the invariant instead of creating duplicate structures.

## Implementation Notes

- Do not infer counts from unauthenticated off-chain arrays.
- Do not rely on database row counts as consensus evidence.
- Every root builder must sort keys canonically before root computation unless
  the structure is explicitly dense-indexed.
- `transition_trace_root` uses dense `step_index` keys; source roots do not.
- `event_to_step_root` is keyed by `EventKey`, not by `step_index`.

## Tests And Verification

Add positive and negative tests for:

- membership proof acceptance for each source root
- non-membership proof acceptance for each source root
- duplicate-key rejection in root builders
- count mismatch rejection
- dense trace range mismatch rejection
- adjacent trace proof for `i` and `i + 1`
- event-to-step membership and non-membership
- TypeScript fixture proof accepted by Aiken

Run:

```sh
cd demo && pnpm run test -- mpf
cd demo && pnpm run test -- cbor-root-normalization
cd demo && pnpm run typecheck
cd ../onchain/aiken && aiken check
```

## Exit Criteria

- The proof layer can express every fault family in the architecture doc.
- A count or range commitment exists for every count stored in `HeaderV2`.
- Aiken and TypeScript agree on all proof encodings.

