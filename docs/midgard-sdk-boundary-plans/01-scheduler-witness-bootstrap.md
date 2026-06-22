# Scheduler Witness Bootstrap Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts` no longer contains
  `ensureRealSchedulerWitnessUtxo`, the scheduler-only bootstrap transaction, or
  a scheduler minting reference-script dependency in the commit witness path.
- `demo/midgard-sdk/src/index.ts` no longer exports the unsafe public
  scheduler-only init API; scheduler root creation remains owned by atomic
  protocol initialization.
- Final live acceptance initialized the hub-oracle and scheduler roots through
  `node dist/index.js init`, tx
  `60247dfe9d82b4393f7cfe1c6d59af36415040a99a1b09c28b03c6b8e57fc25a`, and
  post-init `deployment-status` reported `protocol.complete=true` and
  `stateQueueTopology.healthy=true`.

Verification evidence:
- Source check:
  `rg -n "ensureRealSchedulerWitnessUtxo|incompleteSchedulerInitTxProgram|SchedulerInitParams" demo/midgard-node/src demo/midgard-sdk/src`
  returned no production hits.
- Focused/broad checks passed as recorded in
  `.codex/e2e-reliability-fixes/plan.md`, including scheduler refresh,
  initialization emulator, node typecheck/build, and final live e2e acceptance.

## Problem Statement

`demo/midgard-node/src/workers/utils/scheduler-refresh.ts` currently allows the
commit witness path to create the scheduler root UTxO as a side effect. The
helper `ensureRealSchedulerWitnessUtxo` fetches wallet UTxOs, builds a
node-local transaction, mints `MIDGARD_SCHEDULER`, signs/submits the
transaction, then polls for the new scheduler witness.

That is the wrong ownership boundary and the wrong transaction shape. The
scheduler mint validator accepts `Init | Deinit`, and the on-chain `Init` branch
requires the hub-oracle NFT and scheduler NFT in the same `Transaction.mint`.
The scheduler root must therefore be created only by the canonical atomic
protocol initialization transaction. Scheduler refresh may consume and spend an
existing scheduler UTxO for alignment, but it must never bootstrap protocol
roots during commit preparation.

## Current-State Evidence

- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:507-608` defines
  `ensureRealSchedulerWitnessUtxo`. If no scheduler witness is present, it logs
  a bootstrap message, calls `lucid.wallet().getUtxos()`, selects a fee input,
  creates `lucid.newTx()`, mints `{ [schedulerWitnessUnit]: 1n }`, signs,
  submits, waits for confirmation, and polls
  `lucid.utxosAtWithUnit(...)`.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:539-560` constructs
  the scheduler-only mint transaction in the node worker and uses
  `LucidData.void()` as the scheduler mint redeemer.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:994-1040` fetches a
  `"scheduler minting"` reference script for the commit witness context only so
  the bootstrap helper can use it, while `"scheduler spending"` remains needed
  by scheduler alignment.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:1074-1092` calls
  `ensureRealSchedulerWitnessUtxo` from `fetchRealStateQueueWitnessContext`.
  Missing scheduler root state can therefore trigger protocol-root creation
  during commit preparation.
- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts:610-980` performs
  scheduler alignment by spending an existing scheduler UTxO. That flow remains
  impure and node-owned for now; this plan only removes commit-time root
  bootstrap.
- `onchain/aiken/lib/midgard/scheduler.ak:14-17` defines scheduler
  `MintRedeemer` as `Init | Deinit`.
- `onchain/aiken/validators/scheduler.ak:43-52` requires `Init` to see exactly
  one hub-oracle mint and exactly one scheduler mint in the same
  `Transaction.mint`.
- `demo/midgard-sdk/src/initialization.ts:110-294` builds the SDK atomic
  initialization transaction from explicit inputs. It collects the one-shot
  nonce, mints the hub-oracle NFT, then mints the scheduler NFT with
  `Data.to("Init", SchedulerMintRedeemer)` at
  `demo/midgard-sdk/src/initialization.ts:205`.
- `demo/midgard-node/src/transactions/initialization.ts:678-709` is the node
  wrapper for atomic initialization. It owns impure node concerns such as
  resolving the configured nonce UTxO and deriving DA params, then delegates tx
  construction to `SDK.incompleteInitializationTxProgram`.
- `demo/midgard-node/src/transactions/initialization.ts:529-536` and
  `demo/midgard-node/src/transactions/initialization.ts:739-746` already treat
  partial real deployment as fatal because canonical `Init` validators require
  one atomic bootstrap transaction.
- `demo/midgard-node/src/commands/listen-startup.ts:274-309` already follows
  the desired operational model: complete deployments proceed, partial
  deployments fail, and empty deployments may run atomic initialization only
  when startup bootstrap is explicitly enabled.
- `demo/midgard-sdk/src/scheduler.ts:205-226` defines
  `incompleteSchedulerInitTxProgram`, a standalone scheduler init builder that
  is exported through `demo/midgard-sdk/src/index.ts:20`. Repository search found
  no in-repo runtime caller, but its public export makes the unsafe production
  path discoverable.
- `demo/midgard-node/tests/initialization-emulator.test.ts:190-283` verifies the
  SDK atomic init builder uses explicit inputs and does not fetch wallet UTxOs,
  but currently records only output assets, not the `mintAssets` redeemers.
- `demo/midgard-node/tests/initialization-emulator.test.ts:285-352` proves the
  emulator atomic deployment creates the scheduler UTxO with
  `NoActiveOperators`, and
  `demo/midgard-node/tests/initialization-emulator.test.ts:382-411` verifies a
  hub-oracle-only partial deployment reports missing `scheduler`.

## Target SDK API And Boundary

- SDK pure tx construction:
  `demo/midgard-sdk/src/initialization.ts:110-294` remains the only production
  SDK builder that can mint the scheduler root. It takes explicit initialization
  inputs, uses `Data.to("Init", SchedulerMintRedeemer)`, and does not query
  wallet UTxOs, sign, submit, poll, or inspect node config.
- Node initialization orchestration:
  `demo/midgard-node/src/transactions/initialization.ts:678-709` continues to
  resolve node config, nonce UTxO, DA params, fraud-proof catalogue root, and
  optional reference scripts before calling the SDK atomic builder.
- Node scheduler refresh:
  `demo/midgard-node/src/workers/utils/scheduler-refresh.ts` may fetch and spend
  an existing scheduler UTxO to align the scheduler for a commit. It must not
  mint `MIDGARD_SCHEDULER`, attach/read the scheduler minting script, submit a
  scheduler bootstrap transaction, or repair missing root state.
- Deployment-state semantics:
  a missing scheduler root under a configured deployment is a partial deployment
  error. The commit path should fail before transaction construction or signing;
  only the startup initialization path may create roots, and only from an empty
  deployment.
- Public SDK surface:
  production exports should keep scheduler data/redeemer schemas and fetch
  helpers, but should not expose a standalone scheduler-only init builder. If
  local tests need such a builder, keep it in test support outside the default
  SDK export path and name it explicitly as invalid for production.

## Phased Task Breakdown

1. Add a failing resolver test for missing scheduler root.
   Cover `fetchRealStateQueueWitnessContext` or an extracted resolver in
   `demo/midgard-node/tests/scheduler-refresh.test.ts`. Pass an empty scheduler
   UTxO set and assert a `StateQueueError` whose message identifies an
   incomplete deployment and rejects commit-time scheduler minting. The test
   should spy/stub so `wallet().getUtxos()`, `newTx()`, sign, submit, and
   scheduler mint reference resolution are not called on this missing-root path.

2. Replace `ensureRealSchedulerWitnessUtxo` with an existing-root resolver.
   In `demo/midgard-node/src/workers/utils/scheduler-refresh.ts`, remove the
   bootstrap branch and introduce `requireExistingSchedulerWitnessUtxo` or a
   similarly named helper. It should filter the already fetched scheduler UTxOs
   for `toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME)`, sort
   with `compareOutRefs`, return the canonical existing UTxO, or fail with the
   explicit partial-deployment `StateQueueError`.

3. Remove scheduler-refresh bootstrap dependencies only.
   Delete the scheduler-only minting code, `MIN_SCHEDULER_WITNESS_LOVELACE`, the
   `schedulerMintingScriptRef` parameter, the `"scheduler minting"` entry in the
   `fetchRealStateQueueWitnessContext` reference-script request, and the local
   `optionalReferenceScript("scheduler minting")` lookup. Keep
   `"scheduler spending"` and the scheduler alignment signing/submission helpers,
   because alignment still spends the existing scheduler UTxO.

4. Preserve atomic initialization and strengthen its builder-shape test.
   Extend `demo/midgard-node/tests/initialization-emulator.test.ts:190-283` to
   record every `mintAssets(assets, redeemer)` call. Assert that the hub-oracle
   and scheduler mints occur in the same `SDK.incompleteInitializationTxProgram`
   builder flow, and that the scheduler redeemer equals
   `Data.to("Init", SDK.SchedulerMintRedeemer)`. Keep the existing assertion
   that the builder does not call `wallet()`.

5. Keep emulator deployment and partial-deployment proofs green.
   Preserve or strengthen the test at
   `demo/midgard-node/tests/initialization-emulator.test.ts:285-352` that
   atomic deployment creates the scheduler UTxO with `NoActiveOperators`.
   Preserve the partial deployment test at
   `demo/midgard-node/tests/initialization-emulator.test.ts:382-411` and ensure
   it still reports missing `scheduler`.

6. Retire the standalone scheduler init API from the production SDK surface.
   Re-run `rg -n "incompleteSchedulerInitTxProgram|SchedulerInitParams"` before
   editing. If only `demo/midgard-sdk/src/scheduler.ts` defines them, remove
   `SchedulerInitParams`, `DEFAULT_SCHEDULER_INIT_LOVELACE`, and
   `incompleteSchedulerInitTxProgram`. If an in-repo test needs scheduler-only
   construction, move the helper into that test file or a test-only fixture that
   is not exported from `demo/midgard-sdk/src/index.ts`.

7. Re-check reference-script ownership.
   Keep scheduler minting in atomic init reference-script publication paths such
   as `demo/midgard-node/src/transactions/initialization.ts:175-189` and
   `demo/midgard-node/src/transactions/reference-scripts.ts:1008-1023`.
   Scheduler refresh should no longer request or depend on scheduler minting,
   but deployment metadata and atomic initialization may still publish it.

## Acceptance Criteria

- `demo/midgard-node/src/workers/utils/scheduler-refresh.ts` contains no
  scheduler bootstrap helper, no scheduler-only submit/poll flow, no scheduler
  mint with `LucidData.void()`, and no use of a scheduler minting reference
  script from `fetchRealStateQueueWitnessContext`.
- When scheduler UTxOs are absent, `fetchRealStateQueueWitnessContext` or its
  resolver fails with `StateQueueError` before calling `wallet().getUtxos()`,
  `lucid.newTx()`, sign, submit, or scheduler mint reference resolution.
- Scheduler alignment still works with an existing scheduler UTxO and still uses
  scheduler spending references when available.
- `demo/midgard-sdk/src/initialization.ts` remains the canonical production path
  that mints hub-oracle and scheduler roots together; scheduler mint redeemer is
  `Data.to("Init", SchedulerMintRedeemer)`.
- The default SDK production export path no longer exposes
  `incompleteSchedulerInitTxProgram` or `SchedulerInitParams`. Scheduler
  datum/redeemer schemas, `INITIAL_SCHEDULER_DATUM`, `SCHEDULER_ASSET_NAME`,
  `utxosToSchedulerUTxOs`, and `fetchSchedulerUTxOProgram` remain available.
- Partial deployment behavior remains fatal. No compatibility shim,
  scheduler-only repair transaction, or in-place scheduler bootstrap is
  introduced.
- Reference-script publication still supports scheduler minting for atomic
  initialization and future `Deinit`; scheduler refresh simply stops depending
  on it.

## Tests And Verification

Use targeted checks first, then broaden only as needed:

1. Source-reference searches after editing:
   `rg -n "ensureRealSchedulerWitnessUtxo|LucidData\\.void\\(\\)|schedulerMintingScriptRef|scheduler witness bootstrap|MIDGARD_SCHEDULER.*mintAssets" demo/midgard-node/src/workers/utils/scheduler-refresh.ts`
   should return no scheduler-bootstrap hits.
2. Public SDK search:
   `rg -n "incompleteSchedulerInitTxProgram|SchedulerInitParams" demo/midgard-sdk demo/midgard-node demo/lucid-midgard -g '!node_modules'`
   should return no production source hits after retirement, except any
   deliberately test-only fixture.
3. Focused unit tests:
   `cd demo && pnpm vitest run midgard-node/tests/scheduler-refresh.test.ts`
4. Focused initialization emulator tests:
   `cd demo && pnpm vitest run midgard-node/tests/initialization-emulator.test.ts`
5. Typecheck after implementation:
   `cd demo && pnpm run typecheck`
6. Aiken sanity for the unchanged on-chain invariant:
   `cd onchain/aiken && aiken check`

For this plan-only edit, run only a lightweight Markdown/source-reference
sanity check rather than the TypeScript or Aiken suites.

## Migration And Operational Notes

Existing complete deployments should need no migration. The scheduler UTxO
already created by atomic initialization remains the witness consumed by
scheduler refresh and commit preparation.

Deployments missing the scheduler root must not be repaired in place. Operators
should discard/reset that deployment state, provide a fresh hub-oracle one-shot
nonce UTxO, and rerun the canonical atomic initialization flow from an empty
deployment.

If a commit sees a missing scheduler root immediately after initialization,
treat it as provider visibility lag only inside the bounded visibility wait
owned by the atomic initialization flow
(`demo/midgard-node/src/transactions/initialization.ts:653-676`). Outside that
startup/init window, fail as a partial deployment instead of attempting repair.

Reference-script publication may still include scheduler minting for atomic
initialization and future deinitialization. This change only removes scheduler
refresh's runtime dependency on the scheduler minting script.

## Risks And Open Questions

- Downstream consumers outside this repository may import
  `incompleteSchedulerInitTxProgram`. Decision owner: SDK maintainer. Evidence
  needed: package consumer audit or release-note decision. Until then, prefer a
  breaking removal before launch over preserving a production-looking unsafe
  builder.
- The exact test seam for "no `newTx()` on missing scheduler root" may require
  extracting `requireExistingSchedulerWitnessUtxo` for direct unit testing,
  because `fetchRealStateQueueWitnessContext` also performs unrelated reads
  before scheduler resolution. Decision owner: implementer. Evidence needed:
  smallest test that proves the missing-root path cannot build/sign/submit.
- `Deinit` has the same atomicity shape in Aiken: scheduler burn must pair with
  hub-oracle burn. Decision owner: protocol/SDK maintainer. Evidence needed:
  future deinit design that keeps burn semantics atomic rather than adding a
  standalone scheduler burn helper.
- This plan assumes the current Aiken scheduler validator is the source of
  truth. If protocol design intentionally wants standalone scheduler creation,
  the validator, atomic initialization semantics, node startup policy, and SDK
  public API need a separate design review before implementation.
