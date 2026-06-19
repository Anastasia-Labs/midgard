# Task 08: Challenger Tooling And Node APIs

## Goal

Make transition-trace fraud proofs buildable by independent challengers from DA
and chain data.

This task is complete when a challenger can fetch a committed header, retrieve
DA payload data by `header_hash`, reconstruct roots, detect each fault family,
and submit the corresponding on-chain proof.

## Files To Update

Fault-proof package:

- `demo/midgard-fault-proofs/src/`
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts`
- Add focused modules such as:
  - `demo/midgard-fault-proofs/src/transition-trace/detect.ts`
  - `demo/midgard-fault-proofs/src/transition-trace/witnesses.ts`
  - `demo/midgard-fault-proofs/src/transition-trace/submit.ts`

SDK:

- `demo/midgard-sdk/src/fraud-proofs/transition-trace.ts`
- `demo/midgard-sdk/src/da-payload.ts`
- `demo/midgard-sdk/src/state-queue.ts`

Node/DA APIs:

- `demo/midgard-node/src/workers/commit-block-header/da-payload.ts`
- `demo/midgard-node/src/database/daPayloads.ts`
- `demo/da-committee-node/src/`
- Any libp2p payload exchange module used by production DA.

CLI and tests:

- `demo/midgard-node/src/commands/`
- `demo/midgard-node/tests/`
- `demo/midgard-fault-proofs/tests/`

## Required Challenger Workflows

### Root Reconstruction

Given `header_hash`, fetch DA payload and recompute:

```text
withdrawals_root
forced_transactions_root
transactions_root
deposits_root
transition_trace_root
event_to_step_root
utxos_root
```

Reject payloads that do not match the L1 header.

### Fault Detection

Implement detectors for:

- trace boundary faults
- trace link faults
- event-to-step mismatches
- source membership mismatches
- invalid one-step transitions
- omitted due L1 events
- duplicate trace events
- out-of-window source events
- count faults

The detector should return a proof-kind plus enough witness data to build the
on-chain redeemer.

### Witness Construction

For each proof kind, construct:

- source membership or non-membership proofs
- trace membership proofs
- event-to-step proofs
- L1 evidence proofs for due-event and out-of-window claims
- phase-specific transition witnesses

### Submission

Integrate with the existing fraudulent-block removal flow rather than creating
a parallel removal mechanism unless the current mechanism cannot represent the
new proof token.

## Implementation Notes

- Production DA must not depend on the operator's HTTP endpoint as the only
  data source.
- Local debug endpoints may exist, but tests should exercise the production DA
  path where possible.
- Detection should be deterministic and explain which invariant failed.
- Avoid hiding proof-builder failures behind broad catch-all errors; challenger
  diagnostics need to be precise.

## Tests And Verification

Add tests for:

- reconstruct roots from DA payload v2
- build each witness type from DA payload data
- submit at least one transition-trace fraud proof in emulator
- detector finds a wrong final root caused by an invalid intermediate step
- detector finds omitted forced transaction
- detector finds duplicate trace event
- detector rejects payload whose DA roots do not match header

Run:

```sh
cd demo && pnpm run test -- fraud
cd demo && pnpm run test -- da-payload
cd demo && pnpm run typecheck
```

## Exit Criteria

- Challengers do not need trusted operator cooperation to build proofs.
- Transition-trace proof submission reuses the canonical fraudulent block
  removal lifecycle.
- Error reporting is precise enough to diagnose which invariant failed.

