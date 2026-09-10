# Off-Chain Fault-Proof Reference

Status: Active

Last reviewed: 2026-09-07 (catalogue and installation wiring).

## SDK and catalogue

[`catalogue.ts`](../../demo/midgard-sdk/src/fraud-proof/catalogue.ts) owns
explicit category IDs and presentation order. See [catalogue status](catalogue-status.md)
for the checked source inventory; array position is not category identity.

`demo/midgard-sdk/src/common.ts` and
`demo/midgard-sdk/src/fraud-proof/contracts/` carry the matching contract
types. Node/core deployment manifests and reference-script maps use the same
keys.

State-queue operations use five authenticated rewarding-script references:
commit, unattested-timeout removal, unavailable-timeout removal, fraud removal,
and merge. SDK and node transaction builders supply the arm's indexed
reference-script UTxO and exact zero withdrawal; initialization publishes each
script under its dedicated reference-script-auth role token and registers all
five reward accounts.

## Fault-proof package

`demo/midgard-fault-proofs` provides:

- canonical evidence decoding and retained-DA replay;
- deterministic violation classification;
- family preparation and proof artifact construction;
- field publication/certification and reference-script resolution;
- Init, step, cancel, timeout/award, and removal transaction builders;
- durable journals, funding reservations, retry/resume, and reconciliation;
- manifest-bound production runner admission.

Family modules live under `src/` in `demo/midgard-fault-proofs`; use the
catalogue and their exported contracts/preparation/workflow surfaces to locate a
family, rather than a manually maintained "newest families" list.

## Production runners and watcher installation

`WORKFLOW_RUNNER_FACTORIES` in
[`workflow/runtime.ts`](../../demo/midgard-fault-proofs/src/workflow/runtime.ts)
provides manifest-bound runner factories. The
[watcher application](../../demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts)
composes installations and exposes `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` and
`WATCHER_MISSING_WORKFLOW_CATEGORIES`; the latter is currently empty.

The static adapter registry remains unready until an application supplies an
admitted runner. Runtime startup checks proof readiness, durable recovery,
supervision, and deadlines. See the [watcher README](../../demo/midgard-watcher/README.md)
for the operational CLI. Source installation is distinct from public acceptance.

## Emulator acceptance basis

`tests/support/emulator/protocol-parameters.ts` is the single fault-proof
emulator configuration. It pins Van Rossem's `maxTxSize` to 16,384 bytes,
transaction memory to 16,500,000 units, and transaction CPU to
10,000,000,000 steps. Positive lifecycle tests must submit their real compiled
transactions under those limits; a raised per-test limit is diagnostic only
and cannot establish completion.

## Node and DA integration

- `demo/midgard-node/src/deployment-manifest.ts` binds the complete contract
  and catalogue identity.
- `demo/midgard-node/src/transactions/reference-scripts.ts` publishes the
  required family step scripts.
- `demo/da-committee-node` serves and attests retained `DaPayload` data.
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts` derives and
  submits structural correction.
- Node correction services re-include removed transactions and L1 events after
  confirmation.

## Operational boundary

Release readiness requires all enabled families to have:

1. a concrete public retained-DA/L1 authority;
2. an admitted manifest-bound runner;
3. action-specific funding and durable resume;
4. exact reference-script deployment;
5. terminal state reconciliation;
6. emulator, real-node, and preprod acceptance.
