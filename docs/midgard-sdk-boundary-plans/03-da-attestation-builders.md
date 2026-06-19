# DA Attestation Builder SDK Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- `demo/midgard-sdk/src/da-attestation.ts` exports
  `incompleteInitDaAttestationTxProgram`,
  `incompleteAddDaAttestationSignaturesTxProgram`, and
  `incompleteApplyDaAttestationToStateQueueTxProgram`, plus typed witness
  validation helpers.
- `demo/midgard-node/src/transactions/da-attestation.ts` keeps provider
  discovery, completion fallback policy, signing, submission, confirmation, and
  logging in the node while calling the SDK builders for init, add-signatures,
  and apply transaction assembly.
- Live acceptance produced and applied DA attestations for finalized headers
  `f9310864986614030e8d2005f994b74825639435a8764e252b0ddf6d` and
  `0ef931b270dbb5465b7b9d5a43ad8d047d95eaebfe7b4e0b975d2e9d`.

Verification evidence:
- `demo/midgard-sdk/tests/da-attestation.test.ts` covers witness packing,
  duplicate/out-of-committee/already-attested rejection, init, add-signatures,
  and apply builder behavior.
- Final DB proof recorded two DA payload rows whose roots match the finalized
  pending-journal roots, including final UTxO root
  `38dadfe08e4430229a8890d5610cac743a82058c05825035a6a89e6ff9d68056`.
- SDK/node checks and final live e2e acceptance passed as recorded in
  `.codex/e2e-reliability-fixes/plan.md`.

## Problem Statement

The DA attestation transaction builders are still private to
`demo/midgard-node/src/transactions/da-attestation.ts`, even though they build
protocol transactions from SDK-owned datums, redeemers, state-queue types, and
redeemer-index helpers. This traps reusable protocol transaction construction
behind the node boundary and forces other DA actors to duplicate node internals
or import from a runtime package they should not depend on.

The boundary cleanup should move deterministic DA attestation transaction
assembly into `@al-ft/midgard-sdk` while keeping provider discovery, retry
policy, local seed handling, completion fallback policy, signing, submission,
confirmation, and logging in `midgard-node`.

## Current-State Evidence

- `demo/midgard-node/src/transactions/da-attestation.ts:33-36` defines
  node-local constants for attestation output lovelace, visibility retries, and
  a hard-coded local signer index.
- `demo/midgard-node/src/transactions/da-attestation.ts:75-91` defines
  node-local DA attestation reference-script, state-queue target, and candidate
  shapes that are reusable builder inputs.
- `demo/midgard-node/src/transactions/da-attestation.ts:111-135` contains
  generic datum-CBOR matching and datum decoding helpers used by the private
  builders and fetchers.
- `demo/midgard-node/src/transactions/da-attestation.ts:142-205` and
  `demo/midgard-node/src/transactions/da-attestation.ts:207-326` own
  completion fallback and bootstrap execution-unit behavior. That is operational
  policy, not SDK construction logic.
- `demo/midgard-node/src/transactions/da-attestation.ts:334-578` owns DA
  params fetch, DA attestation candidate fetch, candidate selection, reference
  script fetch, and bitmap helpers in the same module as transaction builders.
- `demo/midgard-node/src/transactions/da-attestation.ts:643-650` signs
  `SDK.daAttestationMessage(headerHash)` and prefixes the signature with the
  node-local signer index.
- `demo/midgard-node/src/transactions/da-attestation.ts:653-745` builds the DA
  attestation `Init` mint transaction.
- `demo/midgard-node/src/transactions/da-attestation.ts:747-815` builds the
  `AddSignatures` spend transaction and updates the bitmap with
  `OPERATOR_DA_SIGNER_INDEX`, so the private builder cannot directly support
  arbitrary committee witnesses.
- `demo/midgard-node/src/transactions/da-attestation.ts:817-935` builds the
  `ApplyToStateQueue` transaction that burns the DA attestation token and
  updates the state-queue datum.
- `demo/midgard-node/src/transactions/da-attestation.ts:937-1111` orchestrates
  fetching, init, local signing, add-signatures, apply, submission, and result
  reporting.
- `demo/midgard-sdk/src/da-attestation.ts:6-104` already owns DA params and
  attestation datum/redeemer schemas, units, the
  `MidgardDAAttestationV1 || header_hash` message, and bitmap constants, but it
  has no transaction builders.
- `demo/midgard-sdk/src/state-queue.ts:168-181` defines the
  `AttachDaAttestation` state-queue spend redeemer and
  `demo/midgard-sdk/src/state-queue.ts:188-192` defines `StateQueueUTxO`.
- `demo/midgard-sdk/src/ledger-state.ts:17-19`,
  `demo/midgard-sdk/src/ledger-state.ts:36-44`, and
  `demo/midgard-sdk/src/ledger-state.ts:71-74` define `HeaderHash`,
  `NO_DA_ATTESTATION`, `StateQueueNode`, and `hashBlockHeader`.
- `demo/midgard-sdk/src/tx-context-redeemer.ts:50-167` exports the final tx
  input, reference-input, output, and redeemer-index helpers used by the node
  builders.
- `demo/midgard-sdk/src/index.ts:4` already re-exports
  `demo/midgard-sdk/src/da-attestation.ts`, so new DA attestation exports there
  become package API.
- `onchain/aiken/lib/midgard/da-attestation-types.ak:64-77` defines
  `AddSignatures.signatures` as packed
  `1-byte signer index || 64-byte Ed25519 signature` chunks with strictly
  ascending indexes.
- `onchain/aiken/validators/da-attestation.ak:122-159` verifies packed
  signatures, writes MSB-first signer bits, and enforces strictly ascending
  signer indexes.
- `onchain/aiken/validators/da-attestation.ak:231-247` recomputes the output
  bitmap and requires `attestation_count` to increase.
- `onchain/aiken/validators/da-attestation.ak:303-342` and
  `onchain/aiken/lib/midgard/state-queue.ak:192-229` enforce threshold,
  DA-token burn, state-queue header match, and `da_attestation` transition to
  the DA attestation policy id during apply.
- Nearby tests cover DA params config
  (`demo/midgard-node/tests/da-attestation-config.test.ts:10-42`), reference
  script registry expectations
  (`demo/midgard-node/tests/reference-scripts.test.ts:24-61`), DA payload
  construction (`demo/midgard-node/tests/da-payload.test.ts:123-230`), and SDK
  state-queue builder style
  (`demo/midgard-sdk/tests/state-queue.test.ts:577-609`), but there is no
  dedicated SDK DA attestation builder test yet.

## Target Architecture Or Target SDK API

### Boundary Decision

Use SDK-owned incomplete transaction builders that return `TxBuilder` values,
matching the existing SDK pattern in `state-queue.ts` and avoiding hidden
provider calls inside the SDK. The SDK should own deterministic datum,
redeemer, witness encoding, bitmap mutation, and transaction assembly. The node
should own all impure orchestration around those builders.

Selected path:

- Add `incomplete*DaAttestation*TxProgram` exports in
  `demo/midgard-sdk/src/da-attestation.ts`.
- Keep `completeWithLocalUplc`, `bootstrapExUnitsEvaluator`, retry/polling,
  seed-to-signature creation, `handleSignSubmit`, and service dependencies in
  `demo/midgard-node/src/transactions/da-attestation.ts`.
- Keep DA params, DA attestation UTxO, state-queue target, and reference-script
  discovery in node for this plan.

Rejected alternatives:

- Do not move the node's completed `TxSignBuilder` wrappers or bootstrap
  execution-unit fallback into the SDK in this pass. That would make an
  operational fallback easier to invoke from non-node callers and broaden a
  risky default path.
- Do not add SDK provider fetchers for DA params, attestation candidates, or
  reference scripts in this pass. Those are useful future APIs, but they mix
  discovery policy with the immediate builder-boundary cleanup.
- Do not preserve private node builder names as compatibility shims. Midgard
  node compatibility is not a goal before launch.

### Proposed SDK Types

Define these in `demo/midgard-sdk/src/da-attestation.ts` unless an existing SDK
shared type is a better fit:

```ts
export class DaAttestationBuildError extends EffectData.TaggedError(
  "DaAttestationBuildError",
)<GenericErrorFields> {}

export type DaAttestationReferenceScripts = {
  readonly daAttestationMinting: UTxO;
  readonly daAttestationSpending: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly stateQueueSpending: UTxO;
};

export type DaAttestationStateQueueTarget = {
  readonly stateQueueUtxo: StateQueueUTxO;
  readonly stateQueueNode: StateQueueNode;
  readonly headerHash: HeaderHash;
};

export type DaAttestationUtxo = {
  readonly utxo: UTxO;
  readonly datum: DaAttestationDatum;
};

export type DaAttestationSignatureWitness = {
  readonly signerIndex: number;
  readonly signatureHex: string;
};
```

`DaAttestationSignatureWitness.signatureHex` is the 64-byte Ed25519 signature
only. The SDK encoder must add the one-byte signer index prefix when building
the packed redeemer bytes.

### Proposed SDK Helpers

Expose or keep internal according to test needs, but implement them in the SDK
so the builders do not depend on node-local helpers:

```ts
export const signerIndexIsDaAttested: (
  attestedSignersHex: string,
  signerIndex: number,
) => boolean;

export const countDaAttestedSigners: (
  attestedSignersHex: string,
) => Effect.Effect<bigint, DaAttestationBuildError>;

export const encodeDaAttestationSignatureWitnesses: (
  witnesses: readonly DaAttestationSignatureWitness[],
) => Effect.Effect<string, DaAttestationBuildError>;

export const applyDaAttestationSignatureWitnesses: (config: {
  readonly attestedSignersHex: string;
  readonly witnesses: readonly DaAttestationSignatureWitness[];
  readonly committeeSize?: number;
}) => Effect.Effect<
  {
    readonly attestedSigners: string;
    readonly attestationCount: bigint;
    readonly packedWitnesses: string;
  },
  DaAttestationBuildError
>;
```

Required helper semantics:

- Reject non-integer, negative, or greater-than-255 signer indexes.
- Reject signatures that are not 128 hex characters.
- Sort witnesses by signer index before packing, or reject unsorted input. The
  selected behavior must be deterministic and documented in tests.
- Reject duplicate witnesses and witnesses whose bit is already set in the
  input bitmap, because on-chain validation requires the count to increase.
- If `committeeSize` is provided, reject signer indexes greater than or equal to
  it.
- Use the same MSB-first bitmap orientation as the current node helper:
  signer index `0` sets the high bit of byte `0`.

### Proposed SDK Builders

```ts
export const incompleteInitDaAttestationTxProgram: (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly target: DaAttestationStateQueueTarget;
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationMinting" | "stateQueueMinting"
    >;
    readonly attestationOutputLovelace: bigint;
  },
) => Effect.Effect<TxBuilder, DaAttestationBuildError>;

export const incompleteAddDaAttestationSignaturesTxProgram: (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation">,
  config: {
    readonly daParamsUtxo: UTxO;
    readonly daParamsDatum: DaParamsDatum;
    readonly attestation: DaAttestationUtxo;
    readonly witnesses: readonly DaAttestationSignatureWitness[];
    readonly referenceScripts: Pick<
      DaAttestationReferenceScripts,
      "daAttestationSpending"
    >;
  },
) => Effect.Effect<TxBuilder, DaAttestationBuildError>;

export const incompleteApplyDaAttestationToStateQueueTxProgram: (
  lucid: LucidEvolution,
  contracts: Pick<MidgardValidators, "daAttestation" | "stateQueue">,
  config: {
    readonly target: DaAttestationStateQueueTarget;
    readonly attestation: DaAttestationUtxo;
    readonly referenceScripts: DaAttestationReferenceScripts;
  },
) => Effect.Effect<TxBuilder, DaAttestationBuildError>;
```

The add-signatures builder should require `daParamsDatum` so it can preflight
the committee size and datum hash/threshold agreement before constructing a
transaction. The apply builder should require decoded attestation datum so it
can fail before completion on header mismatch or below-threshold attestations.

## Phased Task Breakdown

1. Preserve a before/after transaction-shape checklist.

   - In `demo/midgard-node/src/transactions/da-attestation.ts`, record the
     current read inputs, collected inputs, minted/burned units, output datum
     selectors, and redeemer-index helpers for init, add-signatures, and apply.
   - Use the existing private builders as the source of truth while moving code;
     do not simplify transaction shape while crossing the SDK boundary.

2. Add SDK types, validation, and witness helpers.

   - Extend `demo/midgard-sdk/src/da-attestation.ts` with the target types and
     a `DaAttestationBuildError`.
   - Move or recreate canonical datum-output matching, bitmap bit checks, set
     bit counting, packed witness encoding, and witness application helpers.
   - Keep datum decoding/fetching out of this phase; decoded datums are builder
     inputs.
   - Add unit tests for these helpers before migrating node code.

3. Implement `incompleteInitDaAttestationTxProgram`.

   - Build the attestation unit with `daAttestationUnit`.
   - Construct the datum with `EMPTY_ATTESTED_SIGNER_BITMAP` and
     `attestation_count: 0n`.
   - Use `requireOwnMintPurpose`, `requireUniqueOutputIndex`, and
     `requireReferenceInputIndex` exactly as the node builder does for DA
     params, target state-queue UTxO, and state-queue mint reference script.
   - Read from `daParamsUtxo`, `target.stateQueueUtxo.utxo`,
     `referenceScripts.daAttestationMinting`, and
     `referenceScripts.stateQueueMinting`.
   - Do not attach inline scripts or fetch reference scripts from SDK code.

4. Implement `incompleteAddDaAttestationSignaturesTxProgram`.

   - Preflight that the attestation datum matches `daParamsDatum.da_threshold`
     and `daParamsDatum.committee_signers_hash`.
   - Encode witnesses as strictly ascending packed chunks for the redeemer.
   - Update `attested_signers` and `attestation_count` using the helper from
     phase 2, not `OPERATOR_DA_SIGNER_INDEX`.
   - Preserve the input assets exactly in the continued attestation output.
   - Read from `daParamsUtxo` and the DA attestation spending reference script,
     then collect the attestation UTxO with the generated `AddSignatures`
     redeemer.

5. Implement `incompleteApplyDaAttestationToStateQueueTxProgram`.

   - Preflight that `config.attestation.datum.header_hash` equals
     `config.target.headerHash`.
   - Preflight that `attestation_count >= da_threshold`; this supplements
     on-chain validation and should not replace it.
   - Update only the state-queue node's `da_attestation` field to
     `contracts.daAttestation.policyId`.
   - Preserve the state-queue value and linked-list key/next fields.
   - Burn exactly one DA attestation token for the target header.
   - Use the same DA mint, DA spend, and state-queue spend redeemer-index
     relationships as the node builder.

6. Migrate `midgard-node` call sites.

   - Replace private builder calls in `attestHeader` with SDK builder calls.
   - Convert `indexedOperatorSignature` to return
     `DaAttestationSignatureWitness` or a node-local wrapper that is immediately
     converted to that SDK type.
   - Keep signer seed handling, local signer-index selection, DA params fetch,
     candidate selection, visibility retry, completion fallback, transaction
     submission, and logging in node.
   - Complete returned SDK `TxBuilder`s in node with the existing
     `completeWithLocalUplc` options: init uses the current strict local path;
     add-signatures and apply preserve the current provider/bootstrap fallback
     behavior at the call site.

7. Remove node-local duplicates after tests pass.

   - Delete only the private builder helpers and builder-only helper types that
     moved to SDK.
   - Keep node fetchers, selectors, retries, submission helpers, and completion
     fallback.
   - Do not add compatibility aliases for the removed private builder names.

8. Add targeted tests.
   - Add `demo/midgard-sdk/tests/da-attestation.test.ts` for helper semantics
     and builder transaction shape.
   - Add or update node tests proving `attestHeader` or
     `attestStateQueueOnceProgram` routes through SDK builders while preserving
     node-owned orchestration.
   - Prefer emulator or tx-inspection tests that assert datums, assets,
     reference inputs, and redeemer fields over tests that only mock function
     calls.

## Acceptance Criteria

- `@al-ft/midgard-sdk` exports DA attestation builder types and the three
  `incomplete*DaAttestation*TxProgram` functions through the existing
  `src/index.ts` re-export path.
- `demo/midgard-node/src/transactions/da-attestation.ts` no longer defines
  private init, add-signatures, or apply DA attestation transaction builders.
- Node DA attestation orchestration does not call `lucid.newTx()` directly for
  init, add-signatures, or apply; transaction assembly for those flows comes
  from SDK builders.
- SDK code does not import from `demo/midgard-node` or from node aliases.
- The SDK builders do not fetch UTxOs, read environment variables, derive
  wallets from seed phrases, sign payloads, submit transactions, poll for
  visibility, or log service-layer progress.
- Completion fallback remains explicit in node call sites. No SDK builder uses
  bootstrap execution units implicitly.
- Init transaction shape is unchanged: same DA attestation unit, same datum
  fields, same DA params and state-queue reference-input checks, same
  state-queue mint reference-script check, and same attestation output selector.
- Add-signatures transaction shape is unchanged except that signer indexes are
  supplied by typed witness input instead of hard-coded signer index `0`.
- Add-signatures rejects malformed, duplicate, already-attested, out-of-range,
  or committee-out-of-range witnesses before transaction completion.
- Apply transaction shape is unchanged: same DA token burn, same DA spend
  redeemer relation to the DA mint redeemer, same state-queue spend redeemer
  relation to the DA mint redeemer, and same state-queue datum update.
- Applying a DA attestation never changes the state-queue header, key, next
  pointer, assets, or lovelace policy except where current on-chain validation
  already permits output lovelace to increase.
- No compatibility shim preserves old private node builder names.

## Tests And Verification

Targeted implementation checks:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --filter @al-ft/midgard-sdk run typecheck
pnpm --filter @al-ft/midgard-sdk exec vitest run tests/da-attestation.test.ts tests/state-queue.test.ts
pnpm --filter midgard-node run typecheck
pnpm --filter midgard-node exec vitest run tests/da-attestation-config.test.ts tests/reference-scripts.test.ts
```

Add node-side targeted tests if the migration introduces a new
`tests/da-attestation-builders.test.ts` or similar file:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --filter midgard-node exec vitest run tests/da-attestation-builders.test.ts
```

Search assertions after implementation:

```bash
rg -n "const buildInitDaAttestationTx|const buildAddSignaturesTx|const buildApplyAttestationTx" demo/midgard-node/src/transactions/da-attestation.ts
rg -n "lucid\\.newTx\\(" demo/midgard-node/src/transactions/da-attestation.ts
rg -n "bootstrapExUnitsEvaluator|completeWithLocalUplc|walletFromSeed|handleSignSubmit" demo/midgard-sdk/src/da-attestation.ts
rg -n "incompleteInitDaAttestationTxProgram|incompleteAddDaAttestationSignaturesTxProgram|incompleteApplyDaAttestationToStateQueueTxProgram" demo/midgard-sdk/src/da-attestation.ts demo/midgard-sdk/src/index.ts
```

Expected search results:

- The first two commands should return no matches.
- The third command should return no matches.
- The fourth command should show the SDK builder exports in
  `demo/midgard-sdk/src/da-attestation.ts`; `src/index.ts` may only show the
  existing `export * from "./da-attestation.js"` line.

Broader checks before merging, if the targeted checks pass:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm run typecheck
pnpm run test
```

Do not run the full Aiken suite merely for this TypeScript boundary move unless
the implementation changes on-chain schemas, redeemers, or generated contract
artifacts.

## Migration And Operational Notes

- This is an SDK/node code-boundary migration only. It should not require a
  contract redeploy, state reset, database migration, reference-script
  republication, or operator state rewrite.
- Any implementation that changes datums, redeemer schema, token names, policy
  IDs, reference-script names, or state-queue `da_attestation` semantics is out
  of scope for this plan and needs a separate protocol review.
- Keep `DA_ATTESTATION_OUTPUT_LOVELACE` or its configured replacement explicit
  at the node boundary. Do not hide a production funding policy inside the SDK.
- Keep DA signer, L1 submitter, and submitter-only watcher roles separate. SDK
  builders accept already-produced witnesses; they do not assume the caller owns
  a local private signer.
- The SDK builder should build one add-signatures transaction for the witness
  batch supplied by the caller. It should not silently split large batches or
  retry with smaller batches; batching policy belongs to the node or watcher
  planner.
- Rollback before deployment is a normal code revert because no persistent
  on-chain or DB state is migrated. After deployment, rollback should remain
  safe only if transaction shape stayed byte-for-byte equivalent in the fields
  covered by the acceptance criteria.
- Observability remains node-owned. Existing info/warning messages around
  resuming candidates, completion fallback, selected targets, and submitted
  hashes should remain in node or be replaced with equivalent node logs.

## Risks And Open Questions

- Multi-witness batching can exceed transaction-size or execution-unit limits
  even though the on-chain format supports indexed witnesses up to the bitmap
  capacity. Decision owner: DA runtime implementer. Evidence needed: emulator or
  preprod measurements for realistic witness batch sizes using the new SDK
  builder. Until resolved, SDK should not auto-batch or auto-retry.
- The current node uses a hard-coded local signer index of `0`. Decision owner:
  node/operator configuration owner. Evidence needed: intended mapping from
  operator config to DA committee index. This plan only prevents the SDK builder
  from hard-coding index `0`; it does not require a broader node config change.
- Off-chain Ed25519 signature verification is not required for on-chain safety
  because the validator verifies signatures, but early SDK rejection could avoid
  bad submissions. Decision owner: SDK implementer with protocol reviewer.
  Evidence needed: cost and dependency impact of verifying against
  `DaParamsDatum.committee` during builder preflight.
- Existing tests do not inspect DA attestation transaction redeemers directly.
  Decision owner: implementer. Evidence needed: a focused tx-inspection or
  emulator test that can assert the final reference-input and redeemer indexes
  after Lucid completion without depending on unstable CBOR layout details.
