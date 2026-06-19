# Reference-Script Publication SDK Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- Added `demo/midgard-sdk/src/reference-scripts.ts` and exported it through
  `demo/midgard-sdk/src/index.ts`. The SDK now owns reference-script auth token
  definitions, native auth-policy construction/restoration, role-token asset
  derivation, funding target/selection helpers, replenishment transaction
  builders, publication transaction builders, and completed-transaction output
  layout extraction.
- Node source and tests now import reference-script auth helpers directly from
  `@al-ft/midgard-sdk`; the old node-local compatibility shim has been removed.
- `demo/midgard-node/src/transactions/reference-scripts.ts` keeps provider
  fetches, wallet switching, funding policy, retry escalation, signing,
  submission, confirmation, live UTxO reconciliation, and manifest orchestration
  in the node, while calling
  `SDK.completeReferenceScriptWalletReplenishmentTxProgram` and
  `SDK.completeReferenceScriptPublicationTxProgram` for deterministic
  transaction assembly/layout.
- `demo/midgard-node/src/commands/contract-deployment-info.ts` and
  `demo/midgard-node/src/index.ts` avoid blind full redeploys when a valid
  manifest/run-state identity can be attached, and fail with actionable
  diagnostics when stale or invalid state requires a fresh route.
- Live replacement reference-script publication completed with manifest id
  `c5efccce801a924755d446aeb63ce5ea814fb83db24ff0d84d8588f8f4d52130` and log
  `demo/midgard-node/logs/e2e-reference-scripts-retry-20260619T053100Z.log`.

Verification evidence:
- `demo/midgard-sdk/tests/reference-scripts.test.ts`,
  `demo/midgard-node/tests/reference-scripts.test.ts`,
  `tests/contract-deployment-info.test.ts`,
  `tests/reference-script-auth.test.ts`, and `tests/wallet-hygiene.test.ts`
  covered the SDK boundary, publication, manifest, auth-window, and funding
  behavior.
- Final live e2e acceptance used the replacement manifest through init,
  operator registration, deposit, L2 transfers, DA attestation, merge, restart,
  and clean final state as recorded in
  `.codex/e2e-reliability-fixes/plan.md`.

## Problem Statement

Reference-script publication is deployment-time node orchestration, but the
reusable transaction shapes for wallet replenishment and reference-script
publication are currently assembled inside `midgard-node`. That makes a
protocol-significant L1 transaction boundary harder to reuse, test, and audit.

Move deterministic transaction assembly, reference-script role-token derivation,
and publication-output layout extraction into `@al-ft/midgard-sdk`. Keep
provider reads, wallet switching, funding policy, retries, batching, signing,
submission, confirmation, and manifest IO in `midgard-node`.

The target is a source-boundary refactor. It must not change published script
UTxO shape, role-token names, auth-policy semantics, deployment CLI names, or
manifest JSON shape.

## Current-State Evidence

- `demo/midgard-node/src/transactions/reference-scripts.ts` owns target types,
  matching predicates, funding helpers, publication orchestration, and startup
  verification in one module:
  - `ReferenceScriptTarget` and `ReferenceScriptResolved` at lines 30-38.
  - `isSameScriptRef` and `hasReferenceScriptAuthRole` at lines 82-101.
  - provider fetch and startup resolution helpers at lines 103-177 and
    1413-1478.
- Reference-script auth token names and timelock-policy construction are
  node-local in
  `demo/midgard-node/src/deployment/reference-script-auth.ts`:
  - `REFERENCE_SCRIPT_AUTH_TIMELOCK_MS` at line 11.
  - `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` at lines 13-41.
  - token-name/unit helpers at lines 77-96.
  - `createReferenceScriptAuthPolicy` and
    `referenceScriptAuthPolicyDeploymentInfo` at lines 98-143.
- Publication constants and funding selection are node-local:
  - `SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n`,
    `SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n`, and
    `REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n` at
    `demo/midgard-node/src/transactions/reference-scripts.ts:40-42`.
  - publication funding target at lines 233-237.
  - deterministic wallet-input ordering and selection at lines 239-269.
  - plain-wallet filtering that avoids spending reference-script UTxOs at
    lines 227-228 and 447-460.
- Wallet replenishment mixes node-only live behavior with reusable transaction
  shape:
  - live reference-script wallet balance checks and operator-wallet top-up
    policy are in `ensureReferenceScriptWalletWorkingCapital` at lines 530-615.
  - the replenishment transaction is built and completed with
    `fundingLucid.newTx()`, `collectFrom`, `pay.ToAddress`, `coinSelection:
false`, `localUPLCEval: true`, and `presetWalletInputs` at lines 616-628.
  - signing, submission, and post-submit refresh stay node-owned at lines
    635-643.
- Reference-script publication similarly mixes concerns:
  - funding retry state starts at lines 663-679.
  - role-token mint assets are assembled at lines 688-693.
  - `lucid.newTx()`, `collectFrom`, native/non-native `mintAssets`,
    `attach.MintingPolicy`, optional `validTo`, wallet return output, and one
    reference-script output per target are built at lines 694-715.
  - completion with `coinSelection: false`, `localUPLCEval: true`, and
    `presetWalletInputs` happens at lines 716-730.
  - output-layout extraction from the completed transaction body happens at
    lines 731-779.
  - signing/submission, wallet-view restoration, live UTxO reconciliation, and
    provider-omitted-`scriptRef` fallback happen at lines 780-881.
- Target registries are currently node-owned:
  - `nodeRuntimeReferenceScriptTargets` is at lines 884-995.
  - `referenceScriptTargetsByCommand` is at lines 997-1177.
  - the PHAS membership target calls
    `loadPhasMembershipWithdrawalScript()` at lines 976-977 and 1151-1155.
  - the loader itself reads local Aiken blueprint paths and
    `MIDGARD_REAL_BLUEPRINT_PATH` in
    `demo/midgard-node/src/phas-membership.ts:20-49`.
- Deployment CLI and manifest behavior are node-owned today:
  - `deploy-reference-script-${commandName}` is registered in
    `demo/midgard-node/src/index.ts:653-744`.
  - the CLI creates the timelock auth policy, switches wallets, publishes
    reference scripts, fetches live UTxOs, builds deployment info, and writes the
    manifest at lines 663-727.
  - manifest matching by role token and script hash lives in
    `demo/midgard-node/src/commands/contract-deployment-info.ts:200-236`, with
    descriptor collection at lines 238-346 and file writes at lines 490-531.
  - `demo/midgard-node/README.md:204-206` documents
    `deploy-reference-script-node-runtime`; lines 403-414 document
    `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS`, the two-hour timelock, and the
    post-timelock audit rule.
- `@al-ft/midgard-sdk` already owns reusable L1 transaction builders:
  - `incompleteInitializationTxProgram` builds an unsigned protocol init
    `TxBuilder` in `demo/midgard-sdk/src/initialization.ts:110-283`.
  - `incompleteHubOracleInitTxProgram` follows the same `incomplete*TxProgram`
    naming style in `demo/midgard-sdk/src/hub-oracle.ts:173-210`.
  - `completeWithFinalLayoutProgram` demonstrates explicit completion with
    `localUPLCEval: true` and `presetWalletInputs` in
    `demo/midgard-sdk/src/reserve-payout/completion.ts:29-80`.
  - `demo/midgard-sdk/src/index.ts` currently exports no reference-script
    publication module.
- Nearby tests cover registry and verification, not transaction-builder shape:
  `demo/midgard-node/tests/reference-scripts.test.ts` checks command ordering,
  target sets, successful verification, missing-reference diagnostics, and
  rejection of script refs without auth role tokens.

## Target SDK API

Selected path: add `demo/midgard-sdk/src/reference-scripts.ts` and export it
from `demo/midgard-sdk/src/index.ts`.

Rationale:

- The SDK already depends on Lucid and Effect and already owns reusable
  transaction-builder boundaries.
- A separate deployment package would be premature unless deployment CLI,
  manifest writing, and deployment-status logic are extracted together.
- Moving full deployment orchestration into the SDK would pull live provider IO,
  wallet switching, signing, submission, confirmation, and filesystem writes
  across the SDK boundary, which weakens the intended pure-builder separation.

SDK-owned exports:

- Auth policy and token definitions:
  `REFERENCE_SCRIPT_AUTH_TIMELOCK_MS`, `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`,
  `ReferenceScriptAuthTokenTarget`, `ReferenceScriptAuthPolicy`,
  `ReferenceScriptAuthPolicyRef`, `ReferenceScriptAuthMintingPolicy`,
  `ReferenceScriptAuthPolicyDeploymentInfo`,
  `referenceScriptAuthTokenNameText`, `referenceScriptAuthTokenName`,
  `referenceScriptAuthUnit`, `createReferenceScriptAuthPolicy`, and
  `referenceScriptAuthPolicyDeploymentInfo`.
- Target and matching primitives: `ReferenceScriptTarget`,
  `ReferenceScriptResolved`, `isSameScriptRef`,
  `hasReferenceScriptAuthRole`, and `referenceScriptRoleAssets`.
- Constants and pure funding helpers:
  `SCRIPT_REF_OUTPUT_LOVELACE`,
  `SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE`,
  `referenceScriptPublicationFundingTarget`,
  `orderReferenceScriptFundingUtxos`, and
  `selectReferenceScriptFundingUtxos`.
- Replenishment builders:
  `ReferenceScriptWalletReplenishmentTxParams`,
  `incompleteReferenceScriptWalletReplenishmentTxProgram`, and
  `completeReferenceScriptWalletReplenishmentTxProgram`.
- Publication builders and layout helpers:
  `ReferenceScriptPublicationTxParams`, `ReferenceScriptPublicationLayout`,
  `BuiltReferenceScriptPublicationTx`,
  `incompleteReferenceScriptPublicationTxProgram`,
  `resolveReferenceScriptPublicationLayout`, and
  `completeReferenceScriptPublicationTxProgram`.

Expected builder input shape:

- Replenishment input includes `lucid`, explicit `selectedFundingInputs`,
  `referenceScriptAddress`, `topUpAmount`, and no wallet/provider lookup.
- Publication input includes `lucid`, explicit `selectedFundingInputs`,
  `walletAddress`, `referenceScriptsAddress`, `missingTargets`, and
  `authPolicy`.
- Completion programs must pass `coinSelection: false`, `localUPLCEval: true`,
  and `presetWalletInputs: [...selectedFundingInputs]` exactly. These are part
  of the safety boundary, not optional convenience defaults.
- Publication layout extraction must operate on the completed transaction body
  and return wallet outputs plus matched local reference-script outputs keyed by
  target name, preserving the current matching rules: address match,
  `scriptRef` hash match, and exactly one role token for the target.

Node-retained responsibilities:

- CLI command registration and command names.
- Choosing the operator wallet, reference-script wallet, and deploy address.
- Creating the deployment-time auth policy during CLI execution, then passing
  it into SDK builders.
- Loading PHAS membership from the local Aiken blueprint until the validator
  bundle or SDK target registry has an explicit PHAS input.
- Fetching provider-visible UTxOs, wallet snapshots, and live out-ref
  reconciliations.
- Filtering out reference-script UTxOs from spendable wallet candidates.
- Working-capital policy, top-up threshold, funding retries, balance-gap
  parsing, and max-transaction-size batch splitting.
- `lucid.overrideUTxOs(...)` wallet-view management.
- `handleSignSubmit`, confirmation, post-submit refresh, logging, and manifest
  file writing.
- Startup verification and deployment-info export commands.

## Phased Task Breakdown

1. Establish SDK auth-token ownership.
   Move the auth policy/token definitions from
   `demo/midgard-node/src/deployment/reference-script-auth.ts` into the new SDK
   module. Update all node imports to use `@al-ft/midgard-sdk`. Do not leave a
   node-local compatibility re-export shim; the point is to make the SDK the
   single public owner of role-token names and policy deployment metadata.

2. Add SDK target, matching, and funding primitives.
   Move the reusable target types and pure helpers from
   `demo/midgard-node/src/transactions/reference-scripts.ts`. Preserve exact
   names and semantics where exported names already exist. Rename
   `orderWalletFundingUtxos` and `selectWalletFundingUtxos` to
   reference-script-specific SDK names so the API does not look like a generic
   wallet policy. Keep plain-wallet filtering node-local because it depends on
   live wallet views and reference-script spend-safety decisions.

3. Add SDK replenishment construction.
   Implement `incompleteReferenceScriptWalletReplenishmentTxProgram` as the
   current `fundingLucid.newTx().collectFrom(...).pay.ToAddress(...)` shape.
   Implement `completeReferenceScriptWalletReplenishmentTxProgram` as the
   current completion call with explicit preset inputs. The SDK must not decide
   whether replenishment is needed and must not fetch, select, sign, submit, or
   refresh UTxOs.

4. Add SDK publication construction and layout extraction.
   Implement `incompleteReferenceScriptPublicationTxProgram` with the current
   role-token minting, native-policy no-redeemer behavior, non-native
   `Data.void()` redeemer behavior, minting-policy attachment, optional
   `validTo(authPolicy.expiresAtUnixTime)`, one wallet return output, and one
   reference-script output per target. Implement completion and
   `resolveReferenceScriptPublicationLayout` so node can recover the same
   `localReferenceOutputs` and `walletOutputs` it currently derives before
   signing.

5. Refactor node publication orchestration to consume the SDK API.
   In `ensureReferenceScriptWalletWorkingCapital`, keep balance checks,
   same-wallet rejection, funding input selection, logging, signing, submission,
   and refresh in node, but replace direct transaction assembly and completion
   with SDK calls. In `publishMissingReferenceScriptTargets`, keep funding
   retries, `overrideUTxOs`, balance-gap retry, batch splitting, signing,
   submission, wallet-view restoration, and provider fallback in node, but
   replace direct publication assembly/completion/layout parsing with SDK calls.

6. Keep target registries node-local for this pass.
   Leave `REFERENCE_SCRIPT_COMMAND_NAMES`,
   `nodeRuntimeReferenceScriptTargets`, and `referenceScriptTargetsByCommand`
   in node because the PHAS membership target currently depends on
   `loadPhasMembershipWithdrawalScript()` and local blueprint-path resolution.
   If this boundary later moves, add SDK target-builder functions that accept
   `{ phasMembershipWithdrawalScript }` explicitly rather than making the SDK
   read `process.env.MIDGARD_REAL_BLUEPRINT_PATH` or local `plutus.json` paths.

7. Update imports, tests, and docs references narrowly.
   Update node files that currently import `@/deployment/reference-script-auth`
   and tests that import node-local token helpers. Add SDK tests for token
   derivation, funding selection, replenishment builder shape, publication
   builder shape, layout extraction, and native/non-native mint redeemer
   behavior. Keep node tests for command registry, PHAS target coverage,
   startup verification, deployment CLI wiring, and manifest matching.

## Acceptance Criteria

- `demo/midgard-sdk/src/reference-scripts.ts` exists and is exported from
  `demo/midgard-sdk/src/index.ts`.
- `rg -n 'from "@/deployment/reference-script-auth' demo/midgard-node/src demo/midgard-node/tests`
  returns no matches.
- `rg -n 'newTx\\(|\\.complete\\(' demo/midgard-node/src/transactions/reference-scripts.ts`
  shows no node-local construction or completion for reference-script
  replenishment or publication. If another transaction is later added to this
  file, its ownership must be explicitly justified in a nearby comment.
- `rg -n 'handleSignSubmit|utxosAt|wallet\\(\\)\\.getUtxos|overrideUTxOs|writeContractDeploymentInfo' demo/midgard-node/src`
  still shows these live orchestration responsibilities in node.
- Role-token names, token-name hex encoding, role-token units, auth policy id
  derivation, two-hour timelock, `postTimelockAudit` manifest rule, output
  lovelace, publication funding buffer, wallet return output, reference-script
  outputs, validity bound, and completion options are unchanged.
- Native auth policy publication still calls `mintAssets(roleMintAssets)`
  without a redeemer; non-native auth policy publication still calls
  `mintAssets(roleMintAssets, Data.void())`.
- Node command names remain unchanged, especially
  `deploy-reference-script-node-runtime`.
- Manifest JSON remains byte-shape compatible apart from naturally changing
  transaction out refs after new deployments.
- Existing deployments remain valid when role-token names and published UTxO
  shapes match the pre-refactor implementation.
- No SDK API fetches provider UTxOs, reads environment variables, switches
  wallets, signs, submits, waits for confirmation, or writes files for this
  flow.

## Tests And Verification

Add or move focused tests:

- SDK unit tests for `referenceScriptAuthTokenNameText`,
  `referenceScriptAuthTokenName`, `referenceScriptAuthUnit`, and unknown target
  failure behavior.
- SDK unit tests for `referenceScriptPublicationFundingTarget`,
  `orderReferenceScriptFundingUtxos`, and
  `selectReferenceScriptFundingUtxos`, including tie-break by out-ref and
  preference for plain UTxOs over script-ref UTxOs.
- SDK builder tests that inspect completed transaction bodies for
  replenishment outputs, publication role-token minting, wallet return output,
  reference-script outputs, and layout extraction.
- SDK tests for native auth policy publication without a redeemer and
  non-native auth policy publication with `Data.void()`.
- Node tests preserving `demo/midgard-node/tests/reference-scripts.test.ts`
  registry and verification coverage, especially PHAS membership and missing
  auth-token rejection.
- Node tests or existing emulator coverage proving deployment orchestration
  still signs/submits through `handleSignSubmit` and writes the same manifest
  shape.

Targeted implementation checks:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm --dir midgard-sdk run typecheck
pnpm --dir midgard-sdk run test
pnpm --dir midgard-node run typecheck
cd "$(git rev-parse --show-toplevel)/demo/midgard-node"
NODE_ENV=emulator pnpm exec vitest run tests/reference-scripts.test.ts tests/contract-deployment-info.test.ts --reporter=basic --disable-console-intercept
```

Search assertions to run before broader tests:

```bash
cd "$(git rev-parse --show-toplevel)"
rg -n 'from "@/deployment/reference-script-auth' demo/midgard-node/src demo/midgard-node/tests
rg -n 'newTx\(|\.complete\(' demo/midgard-node/src/transactions/reference-scripts.ts
rg -n 'deploy-reference-script-node-runtime|referenceScriptAuthPolicy|L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS' demo/midgard-node/src demo/midgard-node/README.md
```

Broader verification before merge:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm run typecheck
pnpm run test
```

## Migration And Operational Notes

- This refactor should not require redeploying existing reference scripts. A
  redeploy is only required if token names, auth policy metadata, script hashes,
  or reference-script output assets change.
- Keep `L1_REFERENCE_SCRIPT_ADDRESS` as the derived reference-script wallet
  address and keep `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS` as the publication
  destination.
- Keep the same-wallet rejection for replenishment. Operator, merge, and
  reference-script wallets are required to remain operationally distinct in
  `demo/midgard-node/src/services/lucid.ts:769-793`.
- Keep the two-hour native auth timelock and the post-timelock audit
  requirement exactly as documented in `demo/midgard-node/README.md:409-414`.
- Manifest generation remains node-owned because it depends on live provider
  reads, local deployment paths, and the current validator bundle.
- Rollback for a source-boundary mistake is a code rollback only if published
  UTxO shape is unchanged. If a release changes role-token names, policy
  metadata, or output assets, rollback requires explicit deployment guidance and
  a fresh manifest.
- A future `@al-ft/midgard-deployment` package may wrap the SDK builders and
  node orchestration, but it should not own a second copy of raw transaction
  construction.

## Risks And Open Questions

- PHAS membership target ownership remains unresolved. Decision owner: protocol
  SDK maintainers. Evidence needed: whether `MidgardValidators` should include a
  PHAS membership withdrawal script or whether callers should pass
  `phasMembershipWithdrawalScript` into target builders explicitly.
- Moving auth-token definitions into the SDK makes token names a more visible
  public API. Decision owner: release maintainer. Evidence needed: semver or
  migration policy for changing any key in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`.
- Completion APIs in the SDK must expose explicit preset inputs rather than
  silently selecting wallet inputs. Decision owner: SDK implementer. Evidence
  needed: tests that fail if `coinSelection` is enabled or `presetWalletInputs`
  is omitted.
- Provider omission of `scriptRef` is handled today by node-side fallback to the
  locally extracted transaction output. Decision owner: node/runtime
  maintainer. Evidence needed: emulator or mocked-provider test showing the
  fallback still returns the target script when the live provider UTxO has the
  role token but omits `scriptRef`.
- The source-boundary refactor touches deployment-time code. Decision owner:
  release maintainer. Evidence needed before merge: targeted SDK/node checks
  pass and a reviewer confirms no manifest, timelock, or CLI behavior changed.
