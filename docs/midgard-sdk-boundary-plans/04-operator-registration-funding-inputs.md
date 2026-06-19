# Operator Registration Funding Inputs Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- `demo/midgard-sdk/src/operator-lifecycle.ts` now requires
  `registerFundingInputs` in `RegisterOperatorTxConfig` and collects those
  inputs inside `buildRegisterOperatorTx`.
- `demo/midgard-node/src/transactions/register-active-operator.ts` still owns
  wallet UTxO discovery, reference-script exclusion, input selection,
  completion, signing, submission, and confirmation, but passes the selected
  inputs into the SDK builder instead of mutating the returned transaction with
  `.collectFrom([...registerFundingInputs])`.
- Live operator lifecycle completed registration tx
  `f03266e4b12299785c1f91906786d12d14f86b327fc452d7c12d7ac7d0bf475b` and
  activation tx `1732e1c754cef3638c3b98d415fb481512870a2f377d32b5c32f2721547b470d`.

Verification evidence:
- Source check showed `collectFrom([...config.registerFundingInputs])` only in
  the SDK registration builder and no node-side
  `collectFrom([...registerFundingInputs])` mutation.
- Operator lifecycle emulator, SDK transaction-prep, node transaction-prep,
  node typecheck/build, and final live e2e acceptance passed as recorded in
  `.codex/e2e-reliability-fixes/plan.md`.

## Problem Statement

Operator registration transaction construction is almost owned by the SDK, but
the node still mutates the transaction builder returned by the SDK. In
`demo/midgard-node/src/transactions/register-active-operator.ts:912-919`,
`mkRegisterTx` calls `SDK.buildRegisterOperatorTx(...)` and then appends
`.collectFrom([...registerFundingInputs])` in the node layer.

Activation already has the cleaner SDK boundary for this specific funding-input
concern:
`demo/midgard-node/src/transactions/register-active-operator.ts:1183-1207`
passes `activationFundingInputs` into `SDK.buildActivateOperatorTx`, and
`demo/midgard-sdk/src/operator-lifecycle.ts:460-463` collects those inputs
inside the SDK.

This split makes registration easier to diverge from activation and makes the
SDK API understate the full transaction-body inputs required for registration.
The cleanup should move registration funding collection into
`buildRegisterOperatorTx` without changing wallet UTxO discovery, coin
selection, redeemer layout derivation, local UPLC completion, signing, or
submission behavior.

## Current-State Evidence

- `demo/midgard-node/src/transactions/register-active-operator.ts:553-559`
  records the operator lifecycle reference-script outrefs that must be excluded
  from wallet funding input selection.
- `demo/midgard-node/src/transactions/reference-scripts.ts:447-459`
  implements `resolveSpendableWalletUtxos(...)`; it fetches reconciled wallet
  UTxOs, filters to plain wallet UTxOs, and excludes published reference-script
  outrefs so `.readFrom(...)` inputs remain available.
- `demo/midgard-node/src/transactions/reference-scripts.ts:254-269`
  implements `selectWalletFundingUtxos(...)`; it orders wallet UTxOs by the
  local funding heuristic and returns enough inputs to cover the target.
- `demo/midgard-node/src/transactions/register-active-operator.ts:865-888`
  resolves spendable wallet UTxOs, selects `registerFundingInputs`, and fails
  explicitly when no registration funding inputs are available.
- `demo/midgard-node/src/transactions/register-active-operator.ts:912-919`
  builds the registration transaction through the SDK and then mutates the
  returned builder with node-owned `.collectFrom([...registerFundingInputs])`.
- `demo/midgard-node/src/transactions/register-active-operator.ts:932-937` and
  `demo/midgard-node/src/transactions/register-active-operator.ts:960-965` pass
  the same selected inputs as `presetWalletInputs` during both registration
  completion passes with `localUPLCEval: true`.
- `demo/midgard-sdk/src/operator-lifecycle.ts:118-134` defines
  `RegisterOperatorTxConfig` without a registration funding input field.
- `demo/midgard-sdk/src/operator-lifecycle.ts:194-271` builds the registration
  transaction and only collects the registered root script input inside the SDK.
- `demo/midgard-sdk/src/operator-lifecycle.ts:274-286` defines
  `ActivateOperatorTxConfig` with required `activationFundingInputs`.
- `demo/midgard-sdk/src/operator-lifecycle.ts:375-485` shows activation owning
  transaction-body collection for both activation wallet funding inputs and
  operator-list script inputs.
- `demo/midgard-sdk/src/index.ts:13` re-exports `operator-lifecycle.ts`, so the
  config type change is an SDK source API change for direct SDK consumers.
- `demo/midgard-node/package.json:46-48` exposes focused operator lifecycle
  emulator and preprod scripts. The emulator suite includes fragmented-wallet
  and deterministic-churn cases in
  `demo/midgard-node/tests/operator-lifecycle-emulator.test.ts:462-790`.
- `rg -n "buildRegisterOperatorTx" demo --glob '!**/dist/**' --glob '!**/*.tgz'`
  currently finds only the SDK definition and this node call site.

## Target SDK API

Use a breaking, explicit SDK source API change. Add a required field to
`RegisterOperatorTxConfig` in
`demo/midgard-sdk/src/operator-lifecycle.ts`:

```ts
readonly registerFundingInputs: readonly UTxO[];
```

Then make `buildRegisterOperatorTx` collect those inputs internally:

```ts
.collectFrom([...config.registerFundingInputs])
```

Place the new SDK-owned funding collection at the end of the existing
registration builder chain, after `.validTo(Number(config.registerValidTo))`.
That is the closest semantic equivalent to the current node code, which calls
`.collectFrom([...registerFundingInputs])` after `SDK.buildRegisterOperatorTx`
returns. Do not reorder the existing registered-root script input collection
unless a targeted lifecycle test failure proves the current order cannot be
preserved.

The node remains responsible for impure runtime work: wallet UTxO discovery,
reference-script exclusion, funding input selection, completion with
`presetWalletInputs`, signing, submission, logging, and refresh/retry behavior.
The SDK owns pure transaction-body construction once the caller supplies all
body inputs and script/reference context.

Rejected alternatives:

- Keep node-side builder mutation after `SDK.buildRegisterOperatorTx`. This
  preserves the status quo but leaves the SDK API incomplete for registration
  and keeps registration divergent from activation for the same funding-input
  boundary.
- Move wallet lookup or coin selection into the SDK. That would move impure
  node/runtime responsibility into the SDK and couple the SDK to reference
  script publication state.
- Add an optional field, default `[]`, overload, alias, or compatibility shim.
  Midgard is pre-launch production L2 infrastructure; old in-repo API shapes
  should fail at typecheck rather than silently building underfunded
  transactions.

## Phased Task Breakdown

1. Confirm the implementation still has only one non-generated
   `buildRegisterOperatorTx` call site:

   ```bash
   rg -n "buildRegisterOperatorTx" "$(git rev-parse --show-toplevel)/demo" --glob '!**/dist/**' --glob '!**/*.tgz'
   ```

2. In `demo/midgard-sdk/src/operator-lifecycle.ts`, add
   `registerFundingInputs: readonly UTxO[]` to `RegisterOperatorTxConfig`.
   Prefer the name `registerFundingInputs` because it already exists at the
   node call site and matches registration config names such as
   `registerMintAssets` and `registerValidTo`.

3. In `buildRegisterOperatorTx`, collect
   `config.registerFundingInputs` inside the SDK. Preserve the existing
   registration builder chain and append the funding-input collection at the
   current effective point after `.validTo(...)`.

4. Do not change `deriveRegisterLayoutFromContext`, `registerRedeemer`,
   `layout`, or `onLayout` behavior. The current registration layout derives
   reference-input and output indexes; this cleanup should not alter redeemer
   fields or add wallet-input indexes to the layout.

5. In
   `demo/midgard-node/src/transactions/register-active-operator.ts`, pass the
   selected `registerFundingInputs` into `SDK.buildRegisterOperatorTx`. Adding
   the field to `registerTxConfigBase` is preferable if it keeps both layout
   passes using one shared config object.

6. Remove the node-side
   `.collectFrom([...registerFundingInputs])` chained after
   `SDK.buildRegisterOperatorTx(...)`. After the change, registration
   `mkRegisterTx` should only build through the SDK; completion remains outside
   the SDK.

7. Leave node-side wallet input discovery and selection untouched:
   `resolveSpendableWalletUtxos(...)`, `selectWalletFundingUtxos(...)`,
   insufficient-funding errors, lifecycle reference-script exclusions, and
   wallet UTxO logging/failure messages are not part of this boundary move.

8. Leave both registration `.complete(...)` calls using
   `presetWalletInputs: [...registerFundingInputs]` and
   `localUPLCEval: true`. Do not switch to Lucid automatic wallet input
   discovery for registration.

9. Keep activation behavior unchanged except for mechanical typecheck fallout.
   Do not broaden this plan into deregistration; deregistration currently has a
   different completion-only funding pattern in
   `demo/midgard-node/src/transactions/register-active-operator.ts:673-688`.

10. Do not hand-edit generated `dist/` files, packed `.tgz` artifacts, lockfiles,
    on-chain contracts, deployed state, or runtime database files for this
    source cleanup.

## Acceptance Criteria

- `RegisterOperatorTxConfig` has a required
  `registerFundingInputs: readonly UTxO[]` field in
  `demo/midgard-sdk/src/operator-lifecycle.ts`.
- `buildRegisterOperatorTx` collects `config.registerFundingInputs` inside the
  SDK and still collects `config.registeredRootNode.utxo` with
  `LucidData.void()`.
- `demo/midgard-node/src/transactions/register-active-operator.ts` passes the
  selected `registerFundingInputs` into `SDK.buildRegisterOperatorTx`.
- The node no longer appends
  `.collectFrom([...registerFundingInputs])` after the SDK-built registration
  transaction.
- Both registration completion passes still use
  `presetWalletInputs: [...registerFundingInputs]` and `localUPLCEval: true`.
- Registration and activation share the same funding-input ownership model for
  this boundary: node selects wallet inputs, SDK owns transaction-body
  `collectFrom`, node owns completion/signing/submission.
- No SDK or node fallback path allows callers to omit registration funding
  inputs.
- No implementation code moves wallet querying, reference-script exclusion, or
  coin selection into the SDK.
- No activation, deregistration, on-chain, database, state reset, or redeploy
  behavior changes are introduced.
- No generated `dist/` or `.tgz` artifacts are hand-edited.

## Tests And Verification

Run source-level search checks before and after implementation:

```bash
rg -n "buildRegisterOperatorTx" "$(git rev-parse --show-toplevel)/demo" --glob '!**/dist/**' --glob '!**/*.tgz'
rg -n "collectFrom\\(\\[\\.\\.\\.(registerFundingInputs|config\\.registerFundingInputs)\\]\\)" "$(git rev-parse --show-toplevel)/demo" --glob '!**/dist/**'
rg -n "presetWalletInputs: \\[\\.\\.\\.registerFundingInputs\\]|localUPLCEval: true" "$(git rev-parse --show-toplevel)/demo/midgard-node"/src/transactions/register-active-operator.ts
```

Expected post-change search result:

- `buildRegisterOperatorTx` appears in the SDK definition and updated node call
  site, plus any intentional tests added by the implementer.
- `collectFrom([...config.registerFundingInputs])` appears in
  `demo/midgard-sdk/src/operator-lifecycle.ts`.
- `collectFrom([...registerFundingInputs])` does not appear in
  `demo/midgard-node/src/transactions/register-active-operator.ts`.
- Both registration completion passes still pass
  `presetWalletInputs: [...registerFundingInputs]`.

Run targeted TypeScript and lifecycle checks first:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --dir midgard-sdk run typecheck
pnpm --dir midgard-sdk run build
pnpm --dir midgard-node run typecheck
pnpm --dir midgard-node run test:operator-lifecycle:emulator
```

The emulator lifecycle check is important because it covers fragmented wallet
UTxOs and deterministic churn cases that can expose funding-input or redeemer
layout drift.

Run broader checks only if the patch touches shared exports beyond
`operator-lifecycle.ts`, reveals dependency fallout, or the targeted checks
fail in a way that suggests workspace-level impact:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm run typecheck
pnpm run test
```

Preprod validation is environment-gated and optional for the source cleanup,
but recommended before relying on the lifecycle path operationally:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --dir midgard-node run test:operator-lifecycle:preprod
```

Do not run reset or redeploy commands for this cleanup unless a separate
operator explicitly requests a clean environment and follows the repository
state-reset rules.

## Migration And Operational Notes

This is a TypeScript SDK source API change. In-repo consumers should be updated
in the same patch. External consumers that import
`buildRegisterOperatorTx` directly must pass the selected registration funding
UTxOs explicitly after upgrading.

No on-chain migration, database migration, state reset, or redeploy is expected.
The transaction body should remain semantically identical: the same funding
UTxOs selected by the node are collected, only the SDK/node ownership boundary
changes.

Do not introduce a temporary default of `[]`, an optional field, an old-shape
fallback, or an alias such as `registrationFundingInputs`. Midgard production L2
guidance favors explicit, auditable API changes over compatibility shims for
unlaunched in-repo shapes.

The existing register-only and activate-only lifecycle flows remain the
practical verification route. The cleanup should not collapse those operational
paths or hide registration failures behind activation behavior.

If release artifacts are required after implementation, regenerate them through
the package build/repack workflow instead of manual edits. This plan does not
require changing package version metadata.

## Risks And Open Questions

- Funding input order may matter indirectly through Lucid balancing, redeemer
  context construction, collateral choice, or transaction-body determinism. The
  selected plan preserves the current effective call order by appending funding
  collection at the end of `buildRegisterOperatorTx`; emulator lifecycle tests
  are the evidence gate.
- It is not verified here whether Lucid normalizes input ordering enough to make
  the collection call position irrelevant. The implementer should not rely on
  that assumption unless a focused test proves it.
- The SDK currently does not validate non-empty activation funding inputs.
  Adding a non-empty assertion only for registration would create inconsistent
  lifecycle behavior; any shared lifecycle funding invariant should be planned
  separately.
- This plan does not add an SDK-only builder unit test. If a future implementer
  adds one, keep it focused on config ownership and builder shape; do not
  duplicate the full node emulator lifecycle suite in the SDK package.
