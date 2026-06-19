# Scheduler Refresh Builder SDK Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- Added `demo/midgard-sdk/src/scheduler-refresh.ts` and exported
  `encodeSchedulerDatumForChain`, `buildSchedulerRefreshTx`,
  `buildUnsignedSchedulerRefreshTxProgram`, and the refresh config/result types
  through `demo/midgard-sdk/src/index.ts`.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts` now keeps
  witness discovery, timing, signing, submission, polling, and logs in the node,
  while delegating deterministic transaction assembly to
  `SDK.buildUnsignedSchedulerRefreshTxProgram`.
- Node-local builder names such as `CanonicalSchedulerSpendRedeemer` and
  `mkSchedulerRefreshTx` are gone from production source.

Verification evidence:
- `demo/midgard-sdk/tests/scheduler-refresh.test.ts` covers SDK datum encoding,
  final-layout derivation, redeemer construction, reference-input requirements,
  and failure diagnostics.
- Focused and broad checks passed as recorded in
  `.codex/e2e-reliability-fixes/plan.md`, including
  `tests/scheduler-refresh.test.ts`, SDK transaction-prep tests, node
  transaction-prep tests, node typecheck/build, and final live e2e acceptance.

## Problem Statement

The scheduler refresh L1 transaction builder currently lives inside
`midgard-node` even though the exact Lucid transaction shape, scheduler datum
encoding, and scheduler spend redeemer layout are protocol SDK concerns.

The refresh orchestration should remain node-owned: fetching wallet and protocol
UTxOs, parsing linked-list datums, selecting the scheduler witness path, choosing
validity windows, selecting fee input, signing, submitting, polling, and updating
the node's wallet view. The SDK should own the deterministic scheduler refresh
transaction builder and the final redeemer-context index derivation.

This is a boundary refactor, not a protocol behavior change. Preserve strict
production L2 behavior: no demo fallback path, no compatibility toggle, and no
node-local replacement transaction shape.

## Current-State Evidence

- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:57-132` defines
  node-local scheduler refresh selection types, `CanonicalSchedulerDatum`,
  `CanonicalSchedulerSpendRedeemer`, and `encodeSchedulerDatumForChain`.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:200-263` selects
  `Advance`, `AppointFirst`, or `Rewind` from parsed active and registered
  linked-list nodes. This selection still depends on node-fetched datums.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:311-383` computes
  scheduler refresh validity windows and refreshed scheduler start time from the
  local Lucid clock, commit target, and current scheduler datum.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:620-760` owns
  orchestration: wallet view initialization, scheduler datum decoding, node-set
  parsing, witness selection, validity selection, overlap checks, datum
  construction, reference-input list assembly, and fee-input selection.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:761-846` derives
  final scheduler input/output indexes and witness reference-input indexes inside
  a Lucid `BuildTxWithRedeemer` callback.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:854-916` builds the
  refresh transaction with `lucid.newTx()`, completes once to resolve the
  callback redeemer, then rebuilds and completes again with static redeemer CBOR.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:918-980` signs,
  submits, updates the operator wallet view, waits for confirmation, and polls
  for the refreshed scheduler UTxO. These effects should stay in the node.
- `demo/midgard-sdk/src/scheduler.ts:26-150` already owns
  `SCHEDULER_ASSET_NAME`, `SchedulerDatum`, `AdvancingApproach`,
  `SchedulerSpendRedeemer`, and `SchedulerError`.
- `demo/midgard-sdk/src/tx-context-redeemer.ts:50-119` already exposes strict
  final-context helpers: `requireInputIndex`, `requireReferenceInputIndex`,
  `requireUniqueOutputIndex`, and `requireOwnSpendPurpose`.
- `demo/midgard-sdk/src/state-queue-production.ts:382-507` is the closest SDK
  precedent: the node supplies witness context, while the SDK derives final
  layout through Lucid callbacks, completes with local UPLC evaluation, and
  returns a `TxSignBuilder`.
- `demo/midgard-node/tests/scheduler-refresh.test.ts:31-137` currently covers
  node-owned witness selection plus definite-root scheduler datum encoding.
  `demo/midgard-sdk/tests/` currently has no scheduler refresh test file.

## Target Architecture Or Target SDK API

Add `demo/midgard-sdk/src/scheduler-refresh.ts` and export the public builder API
from `demo/midgard-sdk/src/index.ts`. Keep scheduler witness selection in the
node for this pass because the current selection consumes node-decoded
`LinkedListNodeView` values and node policy decisions such as
`allowGenesisRewind`.

The SDK module should expose UTxO-only inputs for the selected refresh path:

```ts
export type SchedulerRefreshNodeWitness = {
  readonly utxo: UTxO;
};

export type SchedulerRefreshWitnessSelection =
  | {
      readonly kind: "Advance";
      readonly activeNode: SchedulerRefreshNodeWitness;
    }
  | {
      readonly kind: "AppointFirst";
      readonly activeNode: SchedulerRefreshNodeWitness;
      readonly registeredWitnessNode: SchedulerRefreshNodeWitness;
    }
  | {
      readonly kind: "Rewind";
      readonly activeNode: SchedulerRefreshNodeWitness;
      readonly activeRootNode: SchedulerRefreshNodeWitness;
      readonly registeredWitnessNode: SchedulerRefreshNodeWitness;
    };

export type SchedulerRefreshLayout =
  | {
      readonly kind: "Advance";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeNodeRefInputIndex: bigint;
    }
  | {
      readonly kind: "AppointFirst";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeNodeRefInputIndex: bigint;
      readonly registeredWitnessRefInputIndex: bigint;
    }
  | {
      readonly kind: "Rewind";
      readonly schedulerInputIndex: bigint;
      readonly schedulerOutputIndex: bigint;
      readonly activeRootRefInputIndex: bigint;
      readonly activeTailRefInputIndex: bigint;
      readonly registeredWitnessRefInputIndex: bigint;
    };

export type BuildSchedulerRefreshTxConfig = {
  readonly lucid: LucidEvolution;
  readonly scheduler: AuthenticatedValidator;
  readonly operatorKeyHash: string;
  readonly feeInput: UTxO;
  readonly schedulerInput: UTxO;
  readonly refreshedDatum: SchedulerDatum;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly selection: SchedulerRefreshWitnessSelection;
  readonly schedulerSpendingScriptRef?: UTxO;
};

export type SchedulerRefreshTxResult = {
  readonly tx: TxSignBuilder;
  readonly layout: SchedulerRefreshLayout;
  readonly schedulerSpendRedeemerCbor: string;
  readonly refreshedDatumCbor: string;
};

export const encodeSchedulerDatumForChain: (datum: SchedulerDatum) => string;

export const buildSchedulerRefreshTx: (
  config: BuildSchedulerRefreshTxConfig,
  schedulerSpendRedeemer: BuildTxWithRedeemer | string,
) => TxBuilder;

export const buildUnsignedSchedulerRefreshTxProgram: (
  config: BuildSchedulerRefreshTxConfig,
) => Effect.Effect<SchedulerRefreshTxResult, SchedulerError>;
```

Boundary rules:

- The node computes `validFrom`, `validTo`, `refreshedDatum`, `feeInput`, and the
  witness selection. The SDK only applies those values to the transaction.
- The SDK derives `schedulerWitnessUnit` from `scheduler.policyId` plus
  `SCHEDULER_ASSET_NAME`, encodes `refreshedDatum` with
  `encodeSchedulerDatumForChain`, and builds the current refresh transaction
  shape.
- The SDK reads witness reference inputs for the selected path and additionally
  reads `schedulerSpendingScriptRef` when present. If no reference script UTxO is
  supplied, it attaches `scheduler.spendingScript`.
- The SDK derives layout from Lucid's final `BuildTxWithRedeemer` context using
  the shared `tx-context-redeemer` helpers. Do not accept caller-supplied layout
  indexes.
- The SDK keeps the current two-pass complete/rebuild behavior initially:
  complete once with a callback redeemer to capture layout and static redeemer
  CBOR, assert the callback resolved exactly once, rebuild with static CBOR, and
  complete again with `{ localUPLCEval: true }`.
- The node maps any `SchedulerError` into its current scheduler-alignment
  `StateQueueError` surface before signing/submitting so worker error reporting
  remains consistent.

## Phased Task Breakdown

1. Create the SDK scheduler refresh module.
   Move `encodeSchedulerDatumForChain` from the node into
   `demo/midgard-sdk/src/scheduler-refresh.ts`. Reuse `SchedulerDatum`,
   `SchedulerSpendRedeemer`, `SCHEDULER_ASSET_NAME`, and
   `normalizeRootIndefiniteArrayEncoding`; do not duplicate scheduler schemas.
   Export the new public API from `demo/midgard-sdk/src/index.ts`.

2. Implement SDK layout and redeemer derivation.
   Add a small internal `deriveSchedulerRefreshLayoutFromRedeemerContext` helper
   that calls `requireOwnSpendPurpose`, `requireInputIndex`,
   `requireReferenceInputIndex`, and `requireUniqueOutputIndex`. The output
   selector must match the current node predicate: scheduler script address,
   exact refreshed datum CBOR, and exactly one scheduler witness token. Encode
   the same three `SchedulerSpendRedeemer` variants:
   `GoToNextDueToEndOfShift`, `AppointFirstOperator`, and
   `RewindDueToEndOfShift`.

3. Implement the SDK transaction builder.
   `buildSchedulerRefreshTx` should reproduce the current node transaction
   chain: `validFrom`, `validTo`, `collectFrom([feeInput])`,
   `readFrom(referenceInputs)`, `collectFrom([schedulerInput], redeemer)`,
   `pay.ToContract(...)` with `schedulerInput.assets`, `addSignerKey`, and
   conditional script attachment. Guard `validFrom` and `validTo` before
   converting bigint milliseconds to Lucid `number` arguments.

4. Implement SDK completion and diagnostics.
   `buildUnsignedSchedulerRefreshTxProgram` should complete with
   `{ localUPLCEval: true }`, fail with `SchedulerError` if the callback did not
   resolve layout or redeemer CBOR, rebuild with static redeemer CBOR, complete
   again with local UPLC evaluation, and return the final `TxSignBuilder`,
   layout, redeemer CBOR, and datum CBOR. Preserve error messages that identify
   build versus rebuild failures.

5. Refactor node usage without moving orchestration.
   In `demo/midgard-node/src/workers/utils/scheduler-refresh.ts`, keep
   `NodeUtxoWithDatum`, `resolveSchedulerRefreshWitnessSelection`, validity
   helpers, overlap checks, fee selection, signing/submission, wallet-view
   updates, confirmation waiting, and polling. Replace `mkSchedulerRefreshTx`,
   `mkSchedulerRefreshTxWithScript`, node-local redeemer construction, and
   node-local datum encoding with `SDK.buildUnsignedSchedulerRefreshTxProgram`.
   Convert node `NodeUtxoWithDatum` selections into the SDK UTxO-only selection
   type at the call site.

6. Move and extend tests.
   Add `demo/midgard-sdk/tests/scheduler-refresh.test.ts` for SDK-owned datum
   encoding, transaction-shape, layout, redeemer, reference-input, and
   script-reference behavior. Keep
   `demo/midgard-node/tests/scheduler-refresh.test.ts` focused on node-owned
   witness selection after moving the encoding assertion to the SDK. Keep
   deposit-flow emulator coverage on the real scheduler refresh path.

## Acceptance Criteria

- `@al-ft/midgard-sdk` exports `encodeSchedulerDatumForChain`,
  `buildSchedulerRefreshTx`, `buildUnsignedSchedulerRefreshTxProgram`, and the
  scheduler refresh config/result/selection/layout types.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts` no longer imports
  `BuildTxWithRedeemer` or Lucid `Data` solely to build the scheduler refresh
  spend redeemer.
- The node file no longer contains `CanonicalSchedulerSpendRedeemer`,
  `mkSchedulerRefreshTx`, `mkSchedulerRefreshTxWithScript`, or a scheduler
  refresh `Data.to(..., SDK.SchedulerSpendRedeemer)` path.
- The refresh transaction still collects the selected fee input, reads exactly
  the selected witness reference inputs plus an optional scheduler spending
  reference script UTxO, spends the scheduler UTxO, recreates the scheduler UTxO
  with identical assets and refreshed datum CBOR, adds the operator signer, and
  attaches the scheduler spending script only when no reference script UTxO is
  supplied.
- The SDK derives layout from the final Lucid transaction context. Missing,
  duplicated, or mismatched scheduler input/output/reference-input elements fail
  loudly with diagnostic `SchedulerError` messages.
- The SDK encodes redeemers identical to the current node behavior for
  `Advance`, `AppointFirst`, and `Rewind`.
- Both completion passes use `.complete({ localUPLCEval: true })`; no path sets
  local UPLC evaluation to false.
- Node signing, submission, wallet-view update, confirmation wait, polling, and
  operator-facing logs remain node-owned and behavior-equivalent.
- No compatibility toggles, legacy aliases, fallback transaction shapes, or
  node-local refresh builders are introduced.

## Tests And Verification

Targeted implementation tests:

- Add `demo/midgard-sdk/tests/scheduler-refresh.test.ts` covering:
  - `encodeSchedulerDatumForChain` preserves the deployed definite-root output
    currently asserted as `d87a8241aa182a`;
  - `Advance`, `AppointFirst`, and `Rewind` layout fields and encoded redeemer
    fields;
  - reference-input requirements for each selection variant, accounting for
    `requireReferenceInputIndex` sorting by out-ref rather than trusting
    `readFrom` array order;
  - missing scheduler input, missing witness reference input, missing refreshed
    output, and duplicate refreshed output failures;
  - script attachment when `schedulerSpendingScriptRef` is absent and reference
    script read behavior when it is present;
  - `validFrom`/`validTo` bigint-to-number guard failures.
- Update `demo/midgard-node/tests/scheduler-refresh.test.ts` so it keeps the
  existing `Advance`, `AppointFirst`, `Rewind`, and invalid-selection coverage
  but no longer imports the datum encoder from node code.
- Keep emulator coverage that exercises scheduler refresh through the real commit
  path, especially `demo/midgard-node/tests/deposit-flow-emulator.test.ts`.

Useful search assertions after implementation:

```bash
cd "$(git rev-parse --show-toplevel)"
rg -n "CanonicalSchedulerSpendRedeemer|mkSchedulerRefreshTx|mkSchedulerRefreshTxWithScript|Data\\.to\\([^\\n]*SchedulerSpendRedeemer|BuildTxWithRedeemer" demo/midgard-node/src/workers/utils/scheduler-refresh.ts
rg -n "buildUnsignedSchedulerRefreshTxProgram|encodeSchedulerDatumForChain" demo/midgard-sdk/src demo/midgard-node/src demo/midgard-sdk/tests demo/midgard-node/tests
```

The first command should have no matches after the refactor. The second command
should show the SDK implementation/export/tests and the node call site.

Verification commands for the future implementation:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --dir midgard-sdk exec vitest run tests/scheduler-refresh.test.ts
pnpm --dir midgard-node exec vitest run tests/scheduler-refresh.test.ts
pnpm --dir midgard-node exec vitest run tests/deposit-flow-emulator.test.ts
pnpm --dir midgard-sdk run typecheck
pnpm --dir midgard-node run typecheck
pnpm run typecheck
pnpm run test
```

Run the targeted package tests first while iterating. Run the broader demo
typecheck and test commands before considering the implementation complete.

## Migration And Operational Notes

- This is a source boundary refactor only. It does not require local state reset,
  redeploy, database migration, or on-chain state migration.
- Preserve the existing deployed scheduler datum encoding unless on-chain
  validator evidence proves every active deployment accepts a different root
  encoding.
- Preserve current reference-script behavior. A present scheduler spending
  reference-script UTxO must be read as a reference input; an absent one must
  attach `scheduler.spendingScript`.
- Preserve node logs around refresh selection, submission, wallet-view update,
  confirmation, and polling unless the SDK result enables strictly better
  structured diagnostics without changing operator workflow.
- Rollback is a code rollback only: revert the SDK builder call site to the
  previous node-local builder if necessary. No protocol state rollback should be
  part of this plan.

## Risks And Open Questions

- Two-pass completion might be avoidable, but the current code relies on it to
  capture static scheduler redeemer CBOR after final layout resolution. Keep it
  until a targeted test proves a single-pass builder is byte- and
  behavior-equivalent.
- Scheduler witness selection could become SDK-owned later, but moving it now
  would also move linked-list datum parsing and policy decisions across the
  boundary. This plan intentionally moves only the transaction builder.
- The definite-root scheduler datum encoder exists for deployed validator
  compatibility. The safe default is to keep it in SDK until validator source and
  live deployment evidence prove it is no longer needed.
- Lucid reference-input ordering is easy to misread. Keep deriving reference
  indexes through `requireReferenceInputIndex`; do not encode indexes from the
  caller's selection array order.
- `SchedulerError` is the right SDK error type because it already exists in
  `scheduler.ts`, but node worker callers currently surface `StateQueueError`.
  The implementation must preserve node-facing error context when mapping SDK
  builder failures.
