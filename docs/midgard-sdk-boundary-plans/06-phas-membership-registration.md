# PHAS Membership Reward Registration SDK Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- Added `demo/midgard-sdk/src/phas-membership.ts` and exported PHAS blueprint
  parsing, reward-address identity derivation, and the deterministic
  reward-account registration transaction builder through
  `demo/midgard-sdk/src/index.ts`.
- `demo/midgard-node/src/transactions/phas-membership-registration.ts` owns
  provider lookup, idempotency/reconciliation, completion, signing, submission,
  and logging while using the SDK-owned PHAS identity and builder surfaces.
- `demo/midgard-node/src/transactions/initialization.ts` now reconciles PHAS
  membership during startup/init; the live init run reported
  `status=already_registered` instead of resubmitting into
  `StakeKeyRegisteredDELEG`.

Verification evidence:
- `demo/midgard-sdk/tests/phas-membership.test.ts` covers blueprint parsing,
  reward identity, and script-stake registration transaction shape.
- `demo/midgard-node/tests/phas-membership-registration.test.ts` and
  `tests/initialization-emulator.test.ts` cover already-present PHAS state and
  provider fallback lookup.
- Final live e2e acceptance passed through init and post-init deployment-status
  using the idempotent PHAS path, as recorded in
  `.codex/e2e-reliability-fixes/plan.md`.

## Problem Statement

PHAS membership reward-account registration is deterministic L1 transaction
construction, but the canonical implementation currently lives in
`midgard-node`.
`demo/midgard-node/src/transactions/phas-membership-registration.ts:51-108`
loads the PHAS membership withdrawal script, derives the reward address and
script hash, builds
`lucid.newTx().register.Stake(rewardAddress).complete({ localUPLCEval: true })`,
signs/submits the transaction, waits for confirmation, and classifies
already-registered provider failures in one node-side program.

That boundary is too wide for production L2 code. The SDK should own the pure,
auditable construction and identity derivation for this reward account; the node
should keep impure concerns: blueprint path discovery, wallet selection,
signing, submission, confirmation, logging, retries, and provider-error
idempotency.

## Current-State Evidence

- `demo/midgard-node/src/transactions/phas-membership-registration.ts:51-108`
  exposes `ensurePhasMembershipRewardAccountRegisteredProgram` and mixes
  construction with submission policy. Lines 58-66 resolve the network, lines
  67-69 load/derive identity, lines 70-81 build the unsigned transaction, and
  lines 82-108 sign/submit/classify the result.
- `demo/midgard-node/src/phas-membership.ts:20-49` performs env/filesystem
  blueprint discovery and PHAS validator extraction from
  `phas.membership.withdraw`. Lines 51-67 duplicate reward-address derivation
  with `CML.RewardAddress`.
- `demo/midgard-sdk/src/cardano-addresses.ts:10-20` already has the generic
  `scriptRewardAddress(network, script)` primitive used by SDK reserve/payout
  internals.
- `demo/midgard-sdk/src/tx-completion.ts:4-10` already centralizes
  `.complete({ localUPLCEval: true })` for SDK builders that return
  `TxSignBuilder`.
- `demo/midgard-sdk/src/errors.ts:40-54` exports `LucidError` for Lucid
  failures and `UnspecifiedNetworkError` for missing network configuration;
  newer SDK builders use `UnspecifiedNetworkError` instead of guessing.
- `demo/midgard-sdk/src/fraud-proof/contracts.ts:101-146` already parses a
  blueprint-like object in the SDK and fails closed on malformed validator
  entries or missing titles.
- `demo/midgard-node/src/transactions/reserve-payout.ts:31-83` shows the
  preferred node wrapper pattern: call an SDK `build...TxProgram`, then use
  node-local `handleSignSubmit`.
- `demo/midgard-node/src/transactions/initialization.ts:591-604` still builds
  PHAS deployment identity in node status reporting by loading the script and
  calling node-local reward-address derivation.
- `demo/midgard-node/tests/initialization-emulator.test.ts:129-161` verifies
  the canonical PHAS script hash, reward address prefix, registration
  certificate shape, and absence of Plutus scripts/redeemers, but it does so by
  building the registration transaction directly in a node test.
- `demo/midgard-node/tests/phas-membership-registration.test.ts:6-28` covers
  only Ogmios `knownCredential` submit-error idempotency, which should remain a
  node concern.
- `demo/midgard-node/tests/reserve-payout-builders.test.ts:326-334` and
  `demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts:942-960`
  contain test-local PHAS/script reward-address helpers that can reuse the SDK
  helper once it is exported. `demo/midgard-fault-proofs/package.json` already
  depends on `@al-ft/midgard-sdk`.
- `docs/agents/transaction-finalization.md` requires local UPLC evaluation for
  transaction completion. `docs/agents/midgard-node.md` and
  `docs/agents/production-l2.md` reject compatibility shims and fallback paths
  for unlaunched node behavior.

## Target Architecture And SDK API

Create `demo/midgard-sdk/src/phas-membership.ts` and export it from
`demo/midgard-sdk/src/index.ts`.

The SDK owns:

- PHAS-specific blueprint shape validation and extraction of the
  `phas.membership.withdraw` Plutus V3 script.
- PHAS membership reward-address and script-hash identity derivation.
- The unsigned reward-account registration transaction builder.

The node owns:

- `MIDGARD_REAL_BLUEPRINT_PATH`, fallback blueprint path discovery, JSON file
  reads, and parse exceptions with source-path context.
- Signing, submission, confirmation, retry/idempotency, CLI logging, and
  operator-facing behavior.

Target public SDK surface:

```ts
export const PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE =
  "phas.membership.withdraw";

export type PhasMembershipBlueprint = {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

export type PhasMembershipIdentity = {
  readonly rewardAddress: string;
  readonly scriptHash: string;
};

export const parsePhasMembershipBlueprint: (
  value: unknown,
) => PhasMembershipBlueprint;

export const phasMembershipWithdrawalScriptFromBlueprint: (
  blueprint: PhasMembershipBlueprint,
) => Script;

export const phasMembershipRewardAddress: (
  network: Network,
  script: Script,
) => string;

export const phasMembershipIdentity: (
  network: Network,
  script: Script,
) => PhasMembershipIdentity;

export type BuiltPhasMembershipRewardRegistrationTx = {
  readonly tx: TxSignBuilder;
  readonly rewardAddress: string;
  readonly scriptHash: string;
};

export type PhasMembershipRewardRegistrationBuildError =
  | LucidError
  | UnspecifiedNetworkError;

export const buildPhasMembershipRewardRegistrationTxProgram: (
  lucid: LucidEvolution,
  config: { readonly script: Script },
) => Effect.Effect<
  BuiltPhasMembershipRewardRegistrationTx,
  PhasMembershipRewardRegistrationBuildError
>;
```

Implementation requirements:

- `parsePhasMembershipBlueprint` should mirror the fail-closed style of
  `parseFaultProofBlueprint`: accept `unknown`, require a JSON object with a
  `validators[]` array, require string `title` and non-empty string
  `compiledCode`, and throw clear `Error`s for malformed input.
- `phasMembershipWithdrawalScriptFromBlueprint` must find exactly the exported
  `PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE` and return
  `{ type: "PlutusV3", script: compiledCode }`.
- `phasMembershipRewardAddress` must delegate to the existing SDK
  `scriptRewardAddress(network, script)` helper. Keep `scriptRewardAddress`
  internal unless a broader SDK public API is deliberately approved.
- `phasMembershipIdentity` must derive `scriptHash` with
  `validatorToScriptHash(script)` and `rewardAddress` with
  `phasMembershipRewardAddress(network, script)`.
- `buildPhasMembershipRewardRegistrationTxProgram` must resolve
  `lucid.config().network`; if missing, fail with `UnspecifiedNetworkError`.
  It must build exactly
  `lucid.newTx().register.Stake(identity.rewardAddress)` and complete through
  `completeTxWithLocalUPLCEvalProgram`, mapping completion failures to
  `LucidError`.
- The SDK builder must not read files, inspect environment variables, select a
  wallet, sign, submit, await confirmation, log, classify provider errors, or
  expose any option to disable local UPLC evaluation.

Rejected or deferred paths:

- Do not keep node-side `newTx().register.Stake(...)` and merely export
  reward-address helpers. That leaves the actual deterministic construction
  outside the SDK boundary.
- Do not move blueprint filesystem discovery into the SDK. It is node/runtime
  configuration IO, not pure SDK construction.
- Do not add a generic blueprint parser as part of this change. A narrow
  PHAS-specific parser is enough and follows the current fault-proof SDK style;
  generic parsing can be revisited after multiple SDK modules need the same
  abstraction.

## Phased Task Breakdown

1. Add `demo/midgard-sdk/src/phas-membership.ts` with the constants, types,
   pure blueprint helpers, identity helpers, and
   `buildPhasMembershipRewardRegistrationTxProgram`.
2. Reuse `scriptRewardAddress` from
   `demo/midgard-sdk/src/cardano-addresses.ts` and
   `completeTxWithLocalUPLCEvalProgram` from
   `demo/midgard-sdk/src/tx-completion.ts`; do not duplicate CML reward-address
   construction or `.complete({ localUPLCEval: true })` plumbing.
3. Export the new module from `demo/midgard-sdk/src/index.ts`.
4. Refactor `demo/midgard-node/src/phas-membership.ts` into a thin adapter:
   keep candidate path resolution and `MIDGARD_REAL_BLUEPRINT_PATH`, parse JSON,
   call `SDK.parsePhasMembershipBlueprint`, then call
   `SDK.phasMembershipWithdrawalScriptFromBlueprint`. Remove node-local
   `CML.RewardAddress` and stake-credential derivation, or replace exports with
   narrow SDK re-exports only where that avoids import churn.
5. Refactor
   `demo/midgard-node/src/transactions/phas-membership-registration.ts` so
   `ensurePhasMembershipRewardAccountRegisteredProgram` loads the script,
   yields `SDK.buildPhasMembershipRewardRegistrationTxProgram(lucid, { script })`,
   then passes `built.tx` to node-local `handleSignSubmit`.
   Preserve the existing result shape and statuses:
   `registration_submitted` and `already_registered`.
6. Keep `isPhasMembershipAlreadyRegisteredError` in node. It depends on
   provider submit error text and operational idempotency, not deterministic SDK
   transaction construction.
7. Refactor `demo/midgard-node/src/transactions/initialization.ts` to use
   `SDK.phasMembershipIdentity(network, phasMembershipScript)` when reporting
   `phasMembershipRewardAddress` and `phasMembershipScriptHash`.
8. Move PHAS registration transaction-shape coverage out of
   `demo/midgard-node/tests/initialization-emulator.test.ts` into a new SDK test
   file. Keep node initialization tests focused on deployment status and
   orchestration.
9. Replace duplicate test-local reward-address helpers where practical:
   `demo/midgard-node/tests/reserve-payout-builders.test.ts` should use
   `SDK.phasMembershipRewardAddress("Preprod" or "Custom", script)` or the
   identity helper, and `demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts`
   may do the same because that package already depends on the SDK.
10. Do not add compatibility aliases, fallback builders, old-node construction
    modes, or operator toggles for this cleanup.

## Acceptance Criteria

- `@al-ft/midgard-sdk` exports PHAS membership helpers and
  `buildPhasMembershipRewardRegistrationTxProgram`.
- `demo/midgard-sdk/src/phas-membership.ts` contains no filesystem, process,
  environment, wallet, signing, submission, confirmation, retry, or logging
  responsibilities.
- `demo/midgard-node/src/transactions/phas-membership-registration.ts` no
  longer calls `lucid.newTx()`, `register.Stake(...)`, or `.complete(...)`
  directly.
- Node PHAS registration orchestration still returns the existing statuses and
  fields: `registration_submitted` with `txHash`, or `already_registered` with
  `txHash: null`, plus `rewardAddress` and `scriptHash`.
- The built transaction remains semantically identical: one stake registration
  certificate for the PHAS membership script credential, no Plutus V1/V2/V3
  script witnesses, no redeemers, and local UPLC evaluation enabled.
- Missing or malformed `phas.membership.withdraw` blueprint data fails closed
  before transaction construction.
- Missing Lucid network fails with `UnspecifiedNetworkError`; Lucid completion
  failures fail with `LucidError`.
- Existing initialization status fields continue reporting the same reward
  address and script hash for the same network and PHAS script.
- No production path preserves old node-side PHAS registration construction as
  an alternate mode.

## Tests And Verification

Add focused SDK tests in `demo/midgard-sdk/tests/phas-membership.test.ts`:

- Parse the real Aiken blueprint object from `onchain/aiken/plutus.json` and
  extract `phas.membership.withdraw`.
- Reject malformed blueprint inputs: non-object input, missing `validators[]`,
  non-string `title`, empty/non-string `compiledCode`, and a blueprint without
  `phas.membership.withdraw`.
- Derive the canonical script hash
  `46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d`.
- Derive a testnet reward address beginning with `stake_test` for
  `"Preprod"`/emulator-style non-mainnet network configuration.
- Build the reward registration transaction through
  `buildPhasMembershipRewardRegistrationTxProgram`, decode it with CML, and
  verify the single stake registration certificate uses a script credential
  equal to the PHAS membership script hash.
- Verify the witness set has no Plutus V1, V2, or V3 scripts and no redeemers.
- Verify a Lucid-like object with `config().network === undefined` fails before
  calling `newTx()`.

Update node and adjacent tests:

- Keep `demo/midgard-node/tests/phas-membership-registration.test.ts` focused on
  provider idempotency classification; add cases only if the refactor changes
  how `scriptHash` reaches the classifier.
- Trim `demo/midgard-node/tests/initialization-emulator.test.ts` so PHAS
  transaction-shape assertions are owned by the SDK test and status assertions
  use `SDK.phasMembershipIdentity`.
- Update `demo/midgard-node/tests/reserve-payout-builders.test.ts` to remove
  the local `CML.RewardAddress` helper when registering zero-reward script
  accounts for PHAS membership proof tests.
- Update `demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts` to use
  the SDK helper if the implementation touches that duplicate registration
  helper; do not expand this into a broader fault-proofs redesign.

Suggested implementation checks:

```bash
cd "$(git rev-parse --show-toplevel)/demo/midgard-sdk"
pnpm run typecheck
pnpm exec vitest run tests/phas-membership.test.ts --reporter=basic

cd "$(git rev-parse --show-toplevel)/demo/midgard-node"
pnpm run typecheck
pnpm exec vitest run tests/phas-membership-registration.test.ts tests/initialization-emulator.test.ts tests/reserve-payout-builders.test.ts --reporter=basic --disable-console-intercept

cd "$(git rev-parse --show-toplevel)/demo/midgard-fault-proofs"
pnpm run typecheck
pnpm exec vitest run tests/submit-init-emulator.test.ts --reporter=basic

cd "$(git rev-parse --show-toplevel)/demo"
pnpm run typecheck
```

Suggested source checks:

```bash
cd "$(git rev-parse --show-toplevel)"
rg -n "newTx\\(|register\\.Stake|\\.complete\\(" demo/midgard-node/src/transactions/phas-membership-registration.ts
rg -n "CML\\.RewardAddress|phasMembershipStakeCredential" demo/midgard-node/src/phas-membership.ts demo/midgard-node/tests/reserve-payout-builders.test.ts demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts
rg -n "MIDGARD_REAL_BLUEPRINT_PATH|readFileSync|existsSync|process\\." demo/midgard-sdk/src/phas-membership.ts
rg -n "buildPhasMembershipRewardRegistrationTxProgram|phasMembershipIdentity|PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE" demo/midgard-sdk/src demo/midgard-node/src demo/midgard-node/tests demo/midgard-sdk/tests
```

Expected results:

- The first command has no matches after refactoring.
- The second command has no PHAS reward-address construction matches except
  unrelated tests that intentionally exercise raw CML address encoding.
- The third command has no matches.
- The fourth command shows the SDK module, SDK export, node call sites, and SDK
  tests.

## Migration And Operational Notes

No on-chain migration is required. Existing PHAS membership reward accounts
remain valid, and already-registered submit failures must remain idempotent in
node orchestration.

Do not reset local state, redeploy contracts, deregister the PHAS reward
account, or change deployment ordering for this boundary cleanup. Atomic
protocol initialization should still submit first, followed by explicit PHAS
reward-account registration in
`demo/midgard-node/src/transactions/initialization.ts`.

Because this change only moves deterministic construction ownership, rollback is
a code rollback, not a chain or database rollback. Follow
`docs/agents/state-reset.md`: never pair a local durable-state wipe with a
previously deployed on-chain protocol state.

Operator-facing CLI/log behavior should be unchanged except for error messages
that now originate from SDK build errors. Preserve the existing successful and
already-registered log fields: status, script hash, reward address, and tx hash
or `already-registered`.

## Risks And Open Questions

- Parser error style: the plan selects pure throwing helpers to match
  `parseFaultProofBlueprint`. If the implementer finds an established
  PHAS/domain-specific tagged error pattern before implementation, use it only
  if every caller and test is updated consistently; otherwise keep the narrow
  existing style.
- Public API width: exporting `phasMembershipRewardAddress` and
  `phasMembershipIdentity` is intentionally narrow. Do not export the generic
  `scriptRewardAddress` unless another SDK consumer needs a non-PHAS public
  helper in the same change.
- Fault-proofs duplicate helper: `demo/midgard-fault-proofs` already depends on
  `@al-ft/midgard-sdk`, so using the SDK PHAS helper is allowed. Keep the change
  limited to the duplicate PHAS reward-registration helper and its test fallout.
- Blueprint drift: the hard-coded canonical script hash is an intentional trip
  wire. If `onchain/aiken/plutus.json` changes, resolve by verifying the Aiken
  contract change, not by silently weakening the test.
- Lucid witness behavior: if a future Lucid version starts attaching certificate
  scripts or redeemers for script stake registration, the SDK transaction-shape
  test must fail before deployment flows silently change.
