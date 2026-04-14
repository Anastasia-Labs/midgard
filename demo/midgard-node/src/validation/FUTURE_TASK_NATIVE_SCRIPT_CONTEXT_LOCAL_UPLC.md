# Future Task: Native Midgard Script-Context Local UPLC Evaluation

## Objective

Replace the current phase-B Plutus validation path that reconstructs Cardano tx
bytes and evaluates them through a whole-transaction phase-two API with a
production-grade local evaluator path that:

- derives the reusable transaction context directly from the native Midgard tx
  plus effective ledger state
- identifies every script purpose that must be validated for that tx
- evaluates each Plutus script locally with an explicit `ScriptContext`
- never relies on provider-side evaluation
- does not make reconstructed Cardano tx bytes the execution boundary

This is a future variant plan. It is not the behavior implemented today.

## Why This Variant Exists

The current implementation shape in `phase-b.ts` still:

- reconstructs Cardano tx bytes from `MidgardNativeTxFull`
- passes those bytes into a whole-transaction evaluator callback
- optionally depends on runtime wiring outside the validator core

That works as a bridge, but it is not the cleanest production design for a
Cardano L2. The stronger design is:

- Midgard-native tx is the source of truth
- effective UTxO state is resolved directly in `midgard-node`
- script purposes are identified explicitly from the native tx and resolved
  state
- the node builds the Cardano-shaped script context data directly
- local UPLC evaluation runs against that explicit context

## Key Clarification

The reusable base is not the full `ScriptContext`. The reusable base is the
transaction-wide `TxInfo`.

Per executed script, the node must vary at least:

- the current redeemer
- the current script purpose / script info

So the intended evaluation model is:

1. build one canonical `TxInfo` from the native Midgard tx and effective state
2. keep the full redeemer map inside that `TxInfo`
3. for each required script execution, construct:
   - `ScriptContext { txInfo, redeemer, purpose }`
4. evaluate the matching script locally

## Current State

Today:

- phase A classifies whether a tx `requiresPlutusEvaluation`
- phase B resolves spent/reference-input state and native-script material
- phase B then reconstructs Cardano tx bytes and calls
  `PhaseBConfig.evaluatePlutusTx`
- the installed JS UPLC surface already includes local whole-tx phase-two eval
  via `eval_phase_two_raw(...)`
- the installed JS surface does not expose a lower-level explicit
  script-by-script evaluation API

The currently exposed WASM entrypoint is:

```ts
eval_phase_two_raw(
  tx_bytes: Uint8Array,
  utxos_bytes_x: Uint8Array[],
  utxos_bytes_y: Uint8Array[],
  cost_mdls_bytes: Uint8Array,
  initial_budget_n: bigint,
  initial_budget_d: bigint,
  slot_config_x: bigint,
  slot_config_y: bigint,
  slot_config_z: number,
): Uint8Array[];
```

That is useful for whole-transaction evaluation, but it is the wrong boundary
for the future design in this document.

## Design Constraints

- No provider evaluation.
- No `.complete({ localUPLCEval: false })` anywhere in this flow.
- No default path that evaluates reconstructed Cardano tx bytes instead of the
  native Midgard tx.
- Deterministic script-purpose discovery from native tx bytes plus effective
  state only.
- Explicit local resolution of script sources, datum sources, redeemers, and
  purpose ordering.
- Ledger ordering semantics must match Cardano exactly where Cardano script
  context expects them:
  - inputs ordered lexicographically by `TxOutRef`
  - reference inputs ordered lexicographically by `TxOutRef`
  - outputs remain in authored order
  - mint redeemer indexes derived from canonical policy ordering
  - observer / withdrawal redeemer indexes derived from canonical reward-account
    ordering
- Native scripts and Plutus scripts must be discovered from the same purpose set
  so witness availability rules stay coherent.

## Non-Goals

- Remote evaluation by Blockfrost, Koios, or any provider.
- Partial Plutus support that bypasses context construction.
- Certifying, voting, or proposing support in this task.
- Collateral, collateral-return, or other Cardano phase-two side channels.
- Silent fallback from explicit-context evaluation to provider evaluation.

## Proposed Architecture

### 1. Add a Lower-Level Local Evaluator Boundary

Create a new internal evaluator module, for example:

- `src/validation/local-plutus-evaluator.ts`

That module should expose a high-level interface like:

```ts
type LocalPlutusEvalArgs = {
  readonly language: "plutus:v1" | "plutus:v2" | "plutus:v3";
  readonly scriptCbor: Uint8Array;
  readonly datumCbor?: Uint8Array;
  readonly redeemerCbor: Uint8Array;
  readonly scriptContextCbor: Uint8Array;
  readonly costModelsCbor: Uint8Array;
  readonly initialBudgetCpu: bigint;
  readonly initialBudgetMem: bigint;
};

type LocalPlutusEvalResult =
  | { readonly kind: "accepted"; readonly remainingCpu: bigint; readonly remainingMem: bigint; readonly traces: readonly string[] }
  | { readonly kind: "script_invalid"; readonly detail: string; readonly traces: readonly string[] };
```

Under the hood, this should be backed by a lower-level WASM function exported
from the UPLC package already in use, rather than by adding a speculative new
VM dependency first.

Recommended raw export to add in the existing evaluator family:

```ts
eval_script_raw(
  language_tag: number,
  script_bytes: Uint8Array,
  arg_bytes: Uint8Array[],
  cost_mdls_bytes: Uint8Array,
  initial_budget_cpu: bigint,
  initial_budget_mem: bigint,
): Uint8Array;
```

Notes:

- `arg_bytes` should be the fully encoded argument list applied to the validator
  in ledger order:
  - spending: `[datum, redeemer, scriptContext]`
  - minting: `[redeemer, scriptContext]`
  - rewarding: `[redeemer, scriptContext]`
- the high-level TS wrapper should decode evaluator output into a stable result
  type and traces
- keep the current `eval_phase_two_raw(...)` available for migration-parity
  tests, but not as the default production validation path

### 2. Build a Canonical Native-to-Cardano `TxInfo`

Add a dedicated builder, for example:

- `src/validation/plutus-script-context.ts`

This builder must derive Cardano-shaped `TxInfo` data directly from:

- the native Midgard tx
- the effective ledger view for the candidate tx
- locally decoded datum/redeemer/script witness material
- explicit slot config and network config

The built `TxInfo` must include at least:

- `inputs`
- `reference_inputs`
- `outputs`
- `fee`
- `mint`
- `wdrl` derived from `requiredObservers`
- `valid_range`
- `signatories`
- `redeemers`
- `data`
- `id`

### 3. Separate Purpose Discovery From Script Source Resolution

The node should not treat inline scripts or reference scripts as the source of
truth for "what must execute".

Instead:

1. discover required script purposes from the tx body plus effective state
2. resolve the matching script source for each required purpose
3. execute or verify the script according to its language

This keeps witness material from being mistaken for purpose declaration.

## How To Identify All Scripts That Must Execute

This is the core of the future design.

### Script-Purpose Discovery Output

Create an internal normalized record like:

```ts
type RequiredScriptExecution = {
  readonly tag: "spend" | "mint" | "reward";
  readonly pointerTag: number;
  readonly pointerIndex: bigint;
  readonly scriptHash: string;
  readonly languageHint: "native" | "plutus" | "unknown";
  readonly datumMode: "none" | "inline" | "hash";
  readonly spendOutRefHex?: string;
  readonly mintPolicyHex?: string;
  readonly rewardCredentialHex?: string;
};
```

The discovery algorithm should populate one such record for every script
purpose that the tx semantically requires.

### A. Spending Scripts

A script execution is required for every spent input whose resolved UTxO address
has a script payment credential.

Algorithm:

1. Resolve every spent input from effective state.
2. Sort the full input set lexicographically by `TxOutRef`.
3. For each sorted input whose payment credential is script:
   - `tag = "spend"`
   - `pointerTag = RedeemerTag.Spend`
   - `pointerIndex = input position in the sorted full input list`
   - `scriptHash = payment credential script hash`
   - `datumMode` is derived from the resolved output datum form

This discovery step is independent of whether the script bytes are inline or
carried by a reference input.

### B. Minting Scripts

A script execution is required for every mint policy present in the tx mint
value.

Algorithm:

1. Decode `mintPreimageCbor`.
2. Extract the set of policy ids with non-zero quantities.
3. Sort policy ids canonically in ledger order.
4. For each policy id:
   - `tag = "mint"`
   - `pointerTag = RedeemerTag.Mint`
   - `pointerIndex = policy position in canonical sorted order`
   - `scriptHash = policy id`
   - `mintPolicyHex = policy id`

Native-script mint policies will be verified with native-script rules.
Plutus-script mint policies will be evaluated through the local UPLC path.

### C. Rewarding Scripts / Observers

Midgard `requiredObservers` semantically represent zero-ADA withdrawals from
script reward accounts.

A script execution is required for every declared required observer.

Algorithm:

1. Decode `requiredObserversPreimageCbor` into canonical observer script hashes.
2. Require an explicit Cardano `networkId`, because rewarding purposes are tied
   to reward accounts.
3. Derive a canonical zero-lovelace withdrawal map keyed by reward account.
4. Order those reward accounts using the same canonical ordering that Cardano
   uses for withdrawal-purpose indexing.
5. For each ordered reward account:
   - `tag = "reward"`
   - `pointerTag = RedeemerTag.Reward`
   - `pointerIndex = reward-account position in canonical withdrawal order`
   - `scriptHash = observer script hash`
   - `rewardCredentialHex = observer script hash`

This discovery step must not treat a reference input that merely contains a
matching `script_ref` as proof that the rewarding script executed. The purpose
comes from `requiredObservers`; the matching script source is resolved later.

### D. Out-Of-Scope Purpose Tags

For this task:

- `Cert`
- `Voting`
- `Proposing`

remain unsupported. If a tx carries redeemers for those tags, the node should
reject explicitly rather than ignore them.

### E. Consistency Checks Between Purposes And Redeemers

After discovering required purposes:

1. Decode the redeemer container.
2. Build the exact expected pointer set from discovered purposes.
3. Reject if:
   - a required Plutus purpose has no matching redeemer
   - a redeemer exists for an unsupported or undiscovered purpose
   - duplicate pointers exist
   - pointer ordering / indexes do not align with canonical discovery

This keeps witness-set redeemers aligned with the actual script purposes of the
native tx.

## Script Source Resolution

After discovering which scripts must execute, resolve script bytes per purpose.

Allowed sources:

- inline script witness in `scriptTxWitsPreimageCbor`
- matching `script_ref` on a reference input

Resolution rules:

1. Match by script hash.
2. Decode language from the resolved script bytes.
3. Reject if no script source exists for a required purpose.
4. Reject if the resolved reference script is non-native / Plutus and the
   purpose requires a datum but datum resolution fails.

This resolution step should produce a normalized per-purpose record containing:

- the discovered purpose
- resolved script bytes
- language
- whether it came from inline witness or reference input

## Datum Resolution

Spending scripts may require datum material.

Rules:

- if the spent output carries inline datum, use that datum directly
- if the spent output carries datum hash only:
  - resolve the datum from `datumTxWitsPreimageCbor`
  - verify the datum hashes to the output datum hash
  - reject if missing or mismatched
- rewarding and minting purposes do not consume datum

The datum set should also populate the `txInfoData` map in the base `TxInfo`.

## Script Integrity Hash Validation

The future design must validate `scriptIntegrityHash` locally rather than only
preserve it.

Before any Plutus execution:

1. collect:
   - redeemers
   - datum witnesses
   - language views required by resolved Plutus scripts
2. compute the script-data hash deterministically
3. compare it with `body.scriptIntegrityHash`
4. reject on mismatch

This must be a first-class validation step, not an incidental by-product of
some other reconstructed-tx path.

## Validity Range In The Script Context

Midgard phase-B validity checks currently treat native validity bounds as slot
numbers.

Cardano `ScriptContext` validity range is time-based. So this future design must
freeze a deterministic slot-to-time mapping for script-context construction.

Recommended rule:

- use explicit local slot configuration
- convert native slot bounds into the corresponding Cardano-valid POSIX range
- use that derived range in the built `TxInfo`

This conversion must be centralized and identical across all nodes.

## `txInfoId` Rule

The future implementation must explicitly freeze how `txInfoId` is derived.

It must not be left implicit.

Recommended direction:

- define one deterministic `txInfoId` mapping for native Midgard evaluation
- keep it stable across all nodes and test fixtures
- ensure exported Cardano-shaped txs and locally built script contexts use the
  same effective rule where compatibility matters

This needs a written spec decision before implementation starts.

## Native Scripts vs Plutus Scripts

This task does not collapse native scripts into the UPLC evaluator.

Recommended split:

- native scripts:
  - keep direct structural verification against signers and validity bounds
  - but discover them from the same `RequiredScriptExecution` set used by
    Plutus purposes
- Plutus scripts:
  - resolve purpose, script source, datum, and redeemer explicitly
  - build `ScriptContext`
  - evaluate locally with the lower-level UPLC entrypoint

This preserves current native-script simplicity while making script discovery
uniform.

## Detailed Plan

### Phase 0. Freeze The Target Semantics

- Specify the canonical purpose-discovery rules for spend, mint, and reward.
- Specify canonical redeemer-index ordering rules for those purposes.
- Specify slot-to-time conversion used for `txInfoValidRange`.
- Specify `txInfoId` derivation.
- Specify script-data-hash computation inputs and serialization rules.

### Phase 1. Introduce New Internal Types And Builders

- Add `RequiredScriptExecution` discovery types.
- Add purpose-discovery helpers from native tx + effective state.
- Add script-source resolution helpers.
- Add datum-resolution helpers.
- Add a base `TxInfo` builder.
- Add per-purpose `ScriptContext` builder.

### Phase 2. Expose A Lower-Level Local UPLC API

- Extend the existing WASM evaluator package to expose `eval_script_raw(...)`
  or equivalent.
- Wrap it in a TS interface that reports accepted/script-invalid/traces/budget.
- Keep the surface local-only and deterministic.
- Add unit tests for direct script invocation with explicit argument lists.

### Phase 3. Integrate With Phase B

- Replace `PhaseBConfig.evaluatePlutusTx` with a local explicit-context
  evaluator interface.
- Build the effective state once per candidate.
- Discover all required script executions.
- Resolve all script sources and datum sources.
- Validate `scriptIntegrityHash`.
- Build one base `TxInfo`.
- Evaluate each Plutus script per discovered purpose.
- Reject on the first script-invalid outcome with stable details.
- Treat evaluator infrastructure failure as retryable infrastructure failure,
  not as a permanent tx rejection.

### Phase 4. Remove Whole-Tx Evaluation As The Default Path

- Stop reconstructing Cardano tx bytes for the production Plutus path.
- Remove provider evaluation from the validation hot path.
- Keep any whole-tx evaluator only as an optional migration-parity harness in
  tests or debug tooling.

### Phase 5. Hardening

- reject extraneous redeemers
- reject extraneous inline Plutus scripts that do not correspond to any
  discovered purpose
- reject malformed datum sets even if not all entries are consumed
- make all ordering helpers shared and reusable by builders/tests
- add structured traces/metrics per executed script purpose

## Task List

- [ ] Write the canonical purpose-discovery spec for spend, mint, and reward.
- [ ] Add native-purpose discovery helpers and tests.
- [ ] Add script-source resolution helpers for inline and reference scripts.
- [ ] Add datum-resolution helpers and hash verification.
- [ ] Add local script-data-hash recomputation and validation.
- [ ] Add deterministic slot-to-time conversion for script-context validity
      ranges.
- [ ] Freeze and implement `txInfoId` derivation for explicit-context eval.
- [ ] Add a base `TxInfo` builder from native tx + effective state.
- [ ] Add a per-purpose `ScriptContext` builder from base `TxInfo`.
- [ ] Extend the local UPLC evaluator package with `eval_script_raw(...)` or
      equivalent.
- [ ] Replace `PhaseBConfig.evaluatePlutusTx` with a local explicit-context
      evaluation interface.
- [ ] Integrate phase B with discovered-purpose execution records.
- [ ] Remove provider evaluation from the production validation path.
- [ ] Add regression tests for spend, mint, and reward Plutus purposes.
- [ ] Add parity tests comparing explicit-context local eval with the current
      whole-tx path during migration.
- [ ] Update validation docs once the implementation becomes authoritative.

## Acceptance Criteria

### Functional

- A Plutus spending script input is evaluated locally against a `ScriptContext`
  built from the native Midgard tx and the resolved spent output, without
  reconstructing a Cardano tx for execution.
- A Plutus minting policy is evaluated locally once per required mint policy
  using canonical policy ordering.
- A Plutus observer / rewarding script is evaluated locally once per declared
  required observer using canonical withdrawal ordering.
- The node executes exactly the set of scripts implied by:
  - script-credential spent inputs
  - non-zero mint policies
  - declared required observers
- Inline scripts and reference scripts are both supported as script sources.
- Datum-hash spending inputs succeed only when the matching datum witness is
  present and hash-consistent.

### Safety

- No required Plutus purpose is skipped because a script witness happened to be
  absent from one source location.
- No extraneous redeemer is silently ignored.
- No extraneous inline Plutus script is silently accepted.
- No provider or remote evaluator is used in the production validation path.
- No path treats a reference script's mere presence as proof that the purpose
  executed.
- `scriptIntegrityHash` is recomputed locally and must match before Plutus
  evaluation proceeds.

### Determinism

- Purpose discovery is a pure function of native tx bytes plus effective state.
- Redeemer indexes are derived deterministically from ledger ordering.
- `TxInfo` construction is deterministic and shared across nodes.
- Slot-to-time conversion for `txInfoValidRange` is explicit and identical
  across nodes.

### Testing

- Unit tests cover purpose discovery for spend, mint, and reward.
- Unit tests cover datum resolution and hash matching.
- Unit tests cover explicit local execution for:
  - spending Plutus scripts
  - minting Plutus scripts
  - rewarding Plutus scripts
- Integration tests cover inline and reference-script variants.
- Migration parity tests compare explicit-context local evaluation with the
  current whole-tx path until the old path is removed.

### Operability

- Validation traces and metrics identify which purpose failed:
  - spend outref
  - mint policy
  - reward credential
- Infrastructure failures in the local evaluator are surfaced distinctly from
  script-invalid outcomes.

## Exit Condition

This future task is complete only when phase-B Plutus validation no longer
depends on reconstructed Cardano tx bytes or provider evaluation, every
required script purpose is discovered explicitly from native Midgard tx plus
effective state, local UPLC evaluation runs against explicit script contexts,
and an independent review concludes that the resulting implementation is
production-grade.
