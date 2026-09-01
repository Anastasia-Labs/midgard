# Fault-Proof Architecture

Current architecture reviewed against the working tree on 2026-09-01.

## End-to-end path

```text
committed HeaderV1
  → retained DaPayloadV1 and authenticated L1 observations
  → deterministic violation classification
  → catalogue membership for the selected first-step validator
  → computation-thread Init
  → one or more authenticated step transitions
  → permanent fault-proof token
  → state-queue descendant pruning and target removal
  → operator slash / prover reward routing
  → node transaction and L1-event re-inclusion
```

The system uses two proof shapes:

- standalone single-party families for faults with bounded direct evidence;
- `validationTraceDispute` for interactive bisection and one-step resolution of
  canonical validation-machine execution, including CEK semantics.

`transitionTrace` is a separate routed family for boundary, link, event,
source, duplicate, omission, window, count, and one-step transition faults.

## Catalogue and deployment identity

`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` in
`demo/midgard-sdk/src/fraud-proof/catalogue.ts` is the positional authority.
It contains 32 categories, `00000000`–`0000001f`. Every deployed family uses
its applied first-step spending-script hash as the catalogue leaf value.

The same topology is represented in:

- SDK `FraudProofs` and catalogue types;
- node/core deployment-manifest identity;
- runtime reference-script deployment entries;
- contract inspection and catalogue membership proofs;
- watcher deployment identity and proof-thread indexer.

Every family step named by deployment identity is consumed as an authenticated
reference script. The catalogue root is
`85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`.

## Computation threads

`onchain/aiken/validators/computation-thread.ak` mints a family-specific thread
NFT for `Init`, advances it between exact step validators, and burns it on
success or authenticated cancellation. Each family step binds:

- the category and challenged header;
- the prover credential;
- the exact predecessor state and expected successor script;
- required field, transaction, UTxO, event, trace, or resolver evidence;
- cancellation to the original prover.

Successful terminal steps burn the thread NFT and mint one permanent
fault-proof token. That token names the deployment, category, and fraudulent
header and has no normal burn path.

## Evidence and carriage

Canonical V1 commitments use counted/domain-tagged roots where count or domain
identity is consensus-relevant. Standalone transaction families bind raw
native CBOR through `pass_native_tx_to_next_step`; transition-trace uses its
own HeaderV1-bound trace claim.

Field preimages use the shared carriage ladder:

1. inline evidence when the complete signed transaction fits;
2. a published raw-UTxO preimage consumed by reference;
3. chunk publication plus an authenticated certificate for larger fields.

All routes fail closed on malformed, non-canonical, reordered, substituted, or
wrong-root evidence. Retained DA is the production block-data source; local
files and diagnostic node surfaces are not production authority.

## Correction and economics

The state-queue minting policy retains only `InitV1` and `Deinit` locally. Its
five operational redeemer arms—commit, unattested-timeout removal,
unavailable-timeout removal, fraud removal, and merge—dispatch to separate
rewarding scripts. Each dispatch selects an indexed reference input carrying
the exact role NFT and script reference, requires a unique zero withdrawal from
that reward account, and requires the arm-specific `YieldStateQueueV1`
withdrawal redeemer. The rewarding script retrieves the original mint
redeemer and performs that arm's complete validation. This keeps each applied
script and publication transaction below the L1 size limit without weakening
the original state transition checks.

`RemoveFraudulentBlockHeader` consumes authenticated state-queue ancestry and
the permanent proof token, prunes descendants structurally across operator
rotation, removes the target header, and routes the exact slash/reward outputs.
The SDK derives a digest-bound correction transition. The node reopens
journaled transactions and L1 events transactionally after confirmed
correction.

The routing logic and compiled economics are non-zero. The testnet profile uses
a 900 ADA required bond, 500 ADA slash penalty, 400 ADA prover reward, and
100 ADA inactivity penalty; the default profile uses 100,000/25,000/75,000/
10,000 ADA respectively. Independent parameter review, exact live balance
conservation, and duplicate-claim idempotency remain release gates.

An unattested head has a separate one-hour, no-slash correction path. That path
is a DA/liveness remedy, not a fault-proof category.

## Off-chain workflow runtime

The fault-proof package performs retained-DA replay, violation classification,
evidence construction, publication, transaction submission, durable journaling,
resume/reconciliation, and removal. It provides 25 production runner factories.

The watcher application installs 25 categories. Missing application
installations are listed in [`offchain-reference.md`](offchain-reference.md).
Classification or topology knowledge alone does not mean a family can be
driven autonomously.

## Trust and release boundaries

- Genesis must publish the intended applied first-step hashes and reference
  scripts; the immutable catalogue cannot repair a bad deployment.
- Challengers require authentic evidence for the full challenge window.
- DA attestation does not by itself provide an on-chain remedy for post-
  attestation withholding.
- The implemented availability-challenge validator is not publishable under
  the current 16,384-byte L1 transaction limit. Its applied spending and
  minting roles are the same 20,017-byte multipurpose script, so the raw body
  alone exceeds the complete-transaction limit. Production publication rejects
  it before funding selection. Activating
  this remedy requires an authenticated split or withdraw-zero-yielding
  redesign, new manifest roles, reward-account registration, and matching
  redeemer/builder ABI changes.
- The aggregate reserve remains an optimistic invariant protected by timely
  fault detection and correction.
- A production escape hatch for a halted operator path remains separate
  liveness work.
- Emulator success establishes transaction fit only when the lifecycle uses
  the shared Van Rossem size and ExUnit limits without a local override.
  Real-node, cross-process, and preprod operation remain separate acceptance
  concerns.
