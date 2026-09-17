# Fault-Proof Architecture

Status: Active

Last reviewed: 2026-09-17 (shared family assembly and recovery).

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

## Family workflow assembly

[Manifest-bound family assembly](workflow-family-assembly.md) centralizes deployment
identity, exact published references, prerequisites, and authenticated completion
for 18 linear and 31 cursor families. Family-specific transaction construction and
historical recovery checks remain in their family modules.

## Watcher proof progress

The watcher supervisor owns one execution slot per deployment, family, and
target header. Callers submit authenticated observations and optional admitted
fault evidence through `requestProgress`; resolution acknowledges admission,
not proof completion. Decision digests and rollback generations are authority
context for that objective, not separate executions.

Each slot holds one invocation and one coalesced pending update. Releasing the
slot and handing over pending work use the same serialized boundary as intake.
A new observation does not mutate the running permit. Explicit loss of chain
authority revokes active and pending permits; handover revalidates authority.

Immediately before funding, the supervisor validates the selected workflow
journal. Retained signed attempts reuse their reservation and exact transaction
records. Provisional completion continues authenticated finality verification.
A completed journal is acknowledged only after canonical terminal verification,
without opening a signer, allocating funding, or invoking the runner. Its
cached verification is bound to the journal revision and canonical generation.
After the new-start deadline, an existing signed attempt can still reconcile
under a separate read-only permit; the deadline does not authorize a fresh spend.

Unfinished objectives are indexed from existing records at restart. Canonical
observations wake affected work; identical observations do not spin the worker.
Admitted native blocks also wake pending finality when queue evidence is
unchanged. This progress marker grants no transaction or finality authority;
the runner still authenticates the retained attempt and terminal observations.
Typed transport failures use capped exponential backoff, and temporary funding
unavailability waits for another observation. Corrupt records, foreign execution
identities, and authentication failures remain actionable hard failures.

Active execution decisions stay pinned in memory; a bounded recent cache covers
late observations of completed objectives. A genuine cache miss opens a fresh
authenticated journal snapshot, since another journal handle may have appended
decisions since startup. Repeated completion checks reuse validated identities.
Decision records are published atomically after their bytes are synced, so a
concurrent authenticated reader sees only complete records.

The workflow journal and signed records remain authoritative. The queue journal
records scheduling activity: a finished queue entry never proves completion of
the objective. No separate persistent objective state machine is introduced.

## Catalogue and deployment identity

`FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` in
`demo/midgard-sdk/src/fraud-proof/catalogue.ts` is the explicit ID authority.
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` controls presentation, not ID derivation.
Every deployed family uses its applied first-step spending-script hash as the
catalogue leaf value. [Catalogue status](catalogue-status.md) owns the inventory.

The same topology is represented in:

- SDK `FraudProofs` and catalogue types;
- node/core deployment-manifest identity;
- runtime reference-script deployment entries;
- contract inspection and catalogue membership proofs;
- watcher deployment identity and proof-thread indexer.

Every family step named by deployment identity is consumed as an authenticated
reference script. Derive the catalogue root from the applied deployment identity;
a root from a prior blueprint is not a current deployment pin.

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

An unattested commitment anywhere in the pending queue has a separate no-slash
correction path once the transaction's inclusive lower bound reaches its
immutable header end time plus one hour. Its immediate predecessor may be the
confirmed root or an immature queued commitment. Both the target and its queue
links are authenticated by the state-queue policy.

The first removal consumes the correction lock, preventing appends and merges
from extending or moving the queue during correction. Each pruning transaction
removes the target's immediate descendant and retains the lock for that target.
Once the target has no descendants, terminal removal preserves its predecessor's
data and value, clears that predecessor's next link, and releases the lock. A
terminal target can be removed in one transaction with the lock returning to
Idle. The earlier prefix remains intact; maturity is not a prerequisite for
discarding the suffix. Attested targets and premature removal reject.

The operator node scans all pending commitments and drives this permissionless,
resumable action; the watcher observes and alerts. Recovery retains signed
attempts before submission, reconciles their exact effects against the canonical
chain, and rebuilds only when the previous attempt is definitively invalidated.
Availability-challenge timeout removal retains its separate head-only rules.
This DA/liveness remedy does not prove fraud or slash the operator.

## Off-chain workflow runtime

The fault-proof package performs retained-DA replay, violation classification,
evidence construction, publication, transaction submission, durable journaling,
resume/reconciliation, and removal. Runner factories are defined in
`demo/midgard-fault-proofs/src/workflow/runtime.ts`; the watcher composes the
complete source catalogue in its application. [Off-chain reference](offchain-reference.md)
links those authorities. Classification or topology knowledge alone does not
establish runtime readiness or successful challenge acceptance.

## Trust and release boundaries

- Genesis must publish the intended applied first-step hashes and reference
  scripts; the immutable catalogue cannot repair a bad deployment.
- Challengers require authentic evidence for the full challenge window.
- DA attestation does not by itself provide an on-chain remedy for post-
  attestation withholding.
- Availability-challenge publication remains a release gate. Its
  [size plan](size-plans/availability-challenge.md) owns the dated measurements
  and required redesign; complete signed publication and lifecycle acceptance
  must establish that the remedy can be deployed.

- The aggregate reserve remains an optimistic invariant protected by timely
  fault detection and correction.
- A production escape hatch for a halted operator path remains separate
  liveness work.
- Emulator success establishes transaction fit only when the lifecycle uses
  the shared Van Rossem size and ExUnit limits without a local override.
  Real-node, cross-process, and preprod operation remain separate acceptance
  concerns.
