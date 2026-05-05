# Plan for Midgard L2 Transaction Evaluation Specification

This plan is for writing a current-state Markdown specification of Midgard L2
transaction validity evaluation only. Use implementation source and tests as
evidence. Do not use existing Markdown or TeX documents as source material for
the spec because they may be stale.

Out of scope for this plan: block inclusion, merge replay, mempool persistence
details after acceptance, status endpoints, readiness, metrics, and target
protocol design.

## End-to-End Flow

Purpose: describe the validity-evaluation path from submitted bytes to an
accepted or rejected validation result.

Gather context from:

- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/listen-utils.ts`
- `demo/midgard-node/src/fibers/tx-queue-processor.ts`
- `demo/midgard-node/src/validation/phase-a.ts`
- `demo/midgard-node/src/validation/phase-b.ts`
- `demo/midgard-node/src/validation/types.ts`

Answer these questions:

- What happens before validation proper?
- Where are submitted bytes normalized into Midgard-native format?
- Where does durable admission end and validation begin?
- What does Phase A decide?
- What does Phase B decide?
- What is the final accepted/rejected output of validity evaluation?

Spec deliverable:

- A concise numbered flow or Mermaid diagram from `POST /submit` through Phase A
  and Phase B.
- Stop the flow at "accepted for mempool insertion" or "rejected."
- Do not continue into block inclusion, merge replay, or observability.

## Midgard-Native Transaction Format

Purpose: document only native transaction format properties that affect
validity evaluation.

Gather context from:

- `demo/midgard-node/src/midgard-tx-codec/native.ts`
- `demo/midgard-node/src/midgard-tx-codec/cbor.ts`
- `demo/midgard-node/src/midgard-tx-codec/hash.ts`
- `demo/midgard-node/src/midgard-tx-codec/output.ts`
- `demo/midgard-node/src/validation/midgard-output.ts`

Answer these questions:

- What native tx version is implemented?
- What is the full transaction CBOR structure?
- Which fields are root/preimage pairs?
- What consistency checks are enforced during decode?
- How is the tx id derived?
- What strict CBOR rules apply?
- How are output bytes decoded for validation?
- How do protected outputs affect later script and receiver checks?

Spec deliverable:

- A current-format section covering version, top-level shape, body and witness
  roots, tx id derivation, strict CBOR, output decoding, and protected-address
  normalization.

## Admission and Ingress

Purpose: describe ingress checks that decide whether submitted bytes can enter
the validation pipeline.

Gather context from:

- `demo/midgard-node/src/commands/listen-router.ts`
- `demo/midgard-node/src/commands/listen-utils.ts`
- `demo/midgard-node/src/database/txAdmissions.ts`

Answer these questions:

- What payload forms does `/submit` accept?
- What hex and size checks happen before decode?
- Does the node attempt Midgard-native decode before Cardano conversion?
- What Cardano-to-native bridge behavior exists?
- What signature domain is required for converted vkey witnesses?
- How are duplicate same-byte txs handled?
- What happens when the same tx id appears with different normalized bytes?

Spec deliverable:

- A short ingress validity gate section.
- State clearly that durable admission is not full tx validation. It normalizes
  and queues eligible bytes for later evaluation.

## Queue Processing

Purpose: describe how admitted txs are selected and configured for validity
evaluation.

Gather context from:

- `demo/midgard-node/src/fibers/tx-queue-processor.ts`
- `demo/midgard-node/src/database/txAdmissions.ts`
- `demo/midgard-node/src/database/mempoolLedger.ts`

Answer these questions:

- How are queued rows claimed?
- What ordering is used for claims?
- What happens to expired leases?
- What batch size and concurrency controls affect validation?
- What config is passed into Phase A?
- What config and pre-state are passed into Phase B?
- When are rows retried instead of rejected?

Spec deliverable:

- A queue-evaluation section focused on batch selection, leases, Phase A config,
  Phase B config, current slot, pre-state map, and retry behavior.

## Phase A Validation

Purpose: describe current stateless per-transaction validation checks.

Gather context from:

- `demo/midgard-node/src/validation/types.ts`
- `demo/midgard-node/src/validation/phase-a.ts`
- `demo/midgard-node/src/validation/midgard-output.ts`
- `demo/midgard-node/src/validation/script-source.ts`
- `demo/midgard-node/src/validation/midgard-redeemers.ts`

Answer these questions:

- Which reject codes can Phase A emit?
- What native decode and consistency assumptions are enforced?
- How is tx id checked?
- How are validity flag, auxiliary data, network id, and fee checked?
- How are spend inputs and reference inputs decoded?
- How are duplicate inputs rejected?
- How are outputs decoded and summed?
- How are required signers and observers decoded?
- How are vkey witnesses verified?
- How are script witnesses classified?
- How are mint and redeemer witnesses decoded?
- Where are datum-hash outputs and Cardano datum witnesses rejected?
- What marks a tx as requiring local script evaluation?

Spec deliverable:

- A narrative Phase A subsection.
- Use current-state language such as "Phase A rejects when..." rather than
  target-design language such as "Phase A should reject when..."

## Phase A Rule Table

Purpose: make Phase A checks auditable and easy to cross-reference.

Use these columns:

- Check
- Source location
- Accepted condition
- Reject code
- Notes

Include rows for:

- Native tx decode
- Tx id/body hash match
- `TxIsValid`
- Auxiliary data forbidden
- Network id
- Minimum fee formula
- Spend input decode
- Empty spends
- Duplicate spends
- Reference input decode
- Reference/spend overlap
- Output decode
- Validity interval format
- VKey witness decode
- Required signer decode
- Required observer decode
- Required signer satisfaction
- Signature over Midgard body hash
- Script witness classification
- Native script validity
- Mint decode
- Redeemer decode
- Datum decode
- Plutus marker/script integrity conditions

Primary source context:

- `demo/midgard-node/src/validation/phase-a.ts`
- `demo/midgard-node/src/validation/types.ts`
- `demo/midgard-node/src/midgard-tx-codec/native.ts`
- `demo/midgard-node/src/validation/midgard-output.ts`

## Phase B Validation

Purpose: describe current stateful and dependency-aware validation checks.

Gather context from:

- `demo/midgard-node/src/validation/phase-b.ts`
- `demo/midgard-node/src/validation/types.ts`
- `demo/midgard-node/src/validation/midgard-output.ts`
- `demo/midgard-node/src/validation/script-source.ts`
- `demo/midgard-node/src/validation/script-context.ts`
- `demo/midgard-node/src/validation/local-script-eval.ts`
- `demo/midgard-node/src/validation/midgard-redeemers.ts`

Answer these questions:

- How is pre-state represented?
- How are in-batch produced outputs visible to dependent txs?
- How is the dependency graph built?
- How are cycles rejected?
- What conflicts are bucketed?
- How are reference inputs resolved?
- How are double spends detected?
- How are pubkey spends authorized?
- How are script spends authorized?
- How are observer scripts and mint policies satisfied?
- How are protected outputs validated?
- How is local script evaluation invoked?
- How is value preservation computed?
- What state patch is produced for accepted txs?

Spec deliverable:

- A narrative Phase B subsection focused on dependency graph, conflict buckets,
  state lookup, witness/script satisfaction, script execution, value
  preservation, and patch production.

## Phase B Rule Table

Purpose: make Phase B checks auditable and easy to cross-reference.

Use these columns:

- Check
- State dependency
- Accepted condition
- Reject code
- Side effect on accept

Include rows for:

- Dependency cycle
- Depends on rejected parent
- Validity interval vs current slot
- Reference input exists
- Reference input not spent by same tx
- Reference output decode
- Reference script extraction
- Required observer material
- Mint policy script material
- Spent input not already spent
- Spent input exists
- Spent output decode
- Pubkey spend witness
- Script spend material
- Datum-hash output rejection
- Extraneous redeemers
- Extraneous script witnesses
- Protected pubkey output witness
- Protected script output receive execution
- Local script evaluation
- Ex-unit budget
- Value preservation
- State patch insert/delete

Primary source context:

- `demo/midgard-node/src/validation/phase-b.ts`
- `demo/midgard-node/src/validation/script-context.ts`
- `demo/midgard-node/src/validation/local-script-eval.ts`
- `demo/midgard-node/src/validation/midgard-redeemers.ts`

## Script Evaluation

Purpose: document current script validity evaluation.

Gather context from:

- `demo/midgard-node/src/validation/script-source.ts`
- `demo/midgard-node/src/validation/script-context.ts`
- `demo/midgard-node/src/validation/local-script-eval.ts`
- `demo/midgard-node/src/validation/phase-b.ts`
- `demo/midgard-node/src/validation/midgard-output.ts`

Answer these questions:

- What script source forms are accepted?
- How are native scripts checked?
- How are PlutusV3 and MidgardV1 script hashes derived?
- How are inline and reference script sources resolved?
- Which script purposes can use PlutusV3?
- Which script purposes require MidgardV1?
- How are contexts built?
- How does Harmonic evaluation determine success or failure?
- How are ex-units enforced?

Spec deliverable:

- A script evaluation subsection separating:
  - native script checks,
  - non-native script source resolution,
  - PlutusV3 context,
  - MidgardV1 context,
  - Harmonic execution,
  - budget enforcement.

## Redeemer and Purpose Indexing

Purpose: document current redeemer pointer and index semantics.

Gather context from:

- `demo/midgard-node/src/validation/midgard-redeemers.ts`
- `demo/midgard-node/src/validation/script-context.ts`
- `demo/midgard-node/src/validation/phase-b.ts`

Answer these questions:

- Which redeemer tags are supported?
- How are redeemers decoded from array and map forms?
- How are duplicate redeemer pointers rejected?
- How are spend redeemer indexes assigned?
- How are mint redeemer indexes assigned?
- How are observer redeemer indexes assigned?
- How are receive redeemer indexes assigned?
- Are spent and reference inputs sorted lexicographically?
- Are outputs preserved in authored order?

Spec deliverable:

- A purpose/indexing table with these columns:
  - Purpose
  - Trigger
  - Redeemer tag
  - Index source
  - Ordering rule
  - Supported script context

Include rows for:

- Spend
- Mint
- Observe
- Receive

## Test Cross-Checks For These Sections

Use tests only as cross-checks for the sections above:

- `demo/midgard-node/tests/native-transaction-integration.test.ts`
- `demo/midgard-node/tests/midgard-native-tx-codec.test.ts`
- `demo/midgard-node/tests/midgard-local-script-eval.test.ts`
- `demo/midgard-node/tests/validation-parallelization.test.ts`
- `demo/midgard-node/tests/listen-admission-auth.test.ts`

Focused verification command:

```sh
pnpm --dir demo/midgard-node test -- tests/native-transaction-integration.test.ts tests/midgard-native-tx-codec.test.ts tests/midgard-local-script-eval.test.ts tests/validation-parallelization.test.ts tests/listen-admission-auth.test.ts
```

## Final Scope Check

Before drafting the actual specification, remove or reject any planned content
about:

- block inclusion,
- merge replay,
- post-acceptance persistence details that do not affect validity,
- status endpoints,
- readiness,
- metrics,
- future design,
- compatibility promises.

Every remaining rule should be backed by source or test evidence and phrased as
current implementation behavior.
