# Fault-Proof Catalogue — Implementation Status

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Status legend:
> **✅ Complete & verified** (real logic + emulator-proven end-to-end) ·
> **🔶 Implemented, not fully verified** (real logic, missing e2e/tooling/registration) ·
> **🟠 Partial** (real core with stubbed/disabled parts) ·
> **📄 Documented but missing** · **❌ Required but undocumented**.

## 1. The thirteen compiled top-level proof families (`onchain/aiken/validators/fraud-proofs/`)

"Registered" = present in the deployment catalogue
(`demo/midgard-sdk/src/fraud-proof/catalogue.ts` — 7 categories). Unregistered
types compile but **cannot `Init` a computation thread** against a deployed instance.
"Tooling" = prepare/submit CLI chain in `demo/midgard-fault-proofs`.

| #   | Type                                                | Fault proven                                                                                                                                                                                                                                                                                                                                          | On-chain logic                    | Registered                               | Off-chain tooling                                                                                                                          | Emulator e2e                                              | Status                                                                                      |
| --- | --------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| 1   | `double-spend` (4 steps + input-witness)            | Two in-block txs spend the same UTxO. Decisive: `tx1_id != tx2_id` + equal spent outref (`step-04.ak:82`).                                                                                                                                                                                                                                            | REAL                              | ✅ `doubleSpend`                         | ✅ `prepare-double-spend` + `submit-init` + `submit-step-01..04` (6 manual CLI steps)                                                      | ✅ through removal (`tests/submit-init-emulator.test.ts`) | ✅ Complete & verified (emulator only; not preprod)                                         |
| 2   | `no-input` (4 steps)                                | Spent input absent from `prev_utxos_root` and not produced in-block. MPF non-membership ×2 (`step-03.ak:71-78`, `step-04.ak:69-76`); authenticates raw root against counted `transactions_root` (`common.ak:611-620`).                                                                                                                                | REAL                              | ✅ `nonExistentInput`                    | ✅ `prepare-non-existent-input` + `ne-submit-step-01..04` (6 manual CLI steps)                                                             | ✅ through removal                                        | ✅ Complete & verified (emulator only)                                                      |
| 3   | `invalid-range` (2 steps)                           | Tx validity interval not covered by block time range, or inverted (`step-02.ak:82-92`).                                                                                                                                                                                                                                                               | REAL                              | ✅ `invalidRange`                        | ✅ `prepare-invalid-range` + `submit-invalid-range-step-01..02`                                                                            | ✅ through removal                                        | ✅ Complete & verified (emulator only)                                                      |
| 4   | `transition-trace` (single proof, 9 fault families) | Wrong state-transition trace: boundary, link, event-to-step, source-membership (3 sub-variants incl. phase mismatch), invalid one-step transition (6 sub-variants), omitted due L1 event, duplicate trace event, out-of-window source event, count faults (5 sub-variants). Dispatch: `lib/midgard/fraud-proofs/transition-trace/proof.ak:1618-1657`. | REAL except one branch            | ✅ `transitionTrace`                     | 🟠 library-complete (`transition-trace/{detect,reconstruct,witnesses,fetch,submit}.ts`), **no CLI wiring** (`bin.ts` never imports it)     | ✅ through removal                                        | 🟠 Partial — see §2                                                                         |
| 5   | `input-no-idx` (4 steps)                            | Input's outref index ≥ producing tx's output count (`step-04.ak:74`).                                                                                                                                                                                                                                                                                 | REAL logic; **legacy binding** ⚠️ | ✅ `nonExistentInputNoIndex`             | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 6   | `zero-input` (2 steps)                              | Tx spends zero inputs; native `spend_inputs_hash` equals the bounded-collection-v1 field-0 empty commitment (`eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec76`, `step-02.ak`).                                                                                                                                                       | REAL; native counted-root binding | ✅ `zeroInput` (`00000005`)              | ✅ `prepare-zero-input` + `submit-zero-input-step-01..02` (4 manual CLI steps); preparation requires the authoritative counted header root | ✅ through removal (`tests/submit-init-emulator.test.ts`) | ✅ Complete & verified (emulator; author-supplied preprod evidence not independently rerun) |
| 7   | `invalid-signature` (2 steps)                       | An ed25519 signature fails verification (`step-02.ak:82-87`). Open TODO: duplicate-vkey manipulation (`:75-76`). Also parses witnesses as a `Pairs` map vs the codec's list-of-arrays (matrix §11 #11).                                                                                                                                               | REAL logic; **legacy binding** ⚠️ | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 8   | `missing-signature` (4 steps)                       | Required signer's witness absent (`step-04.ak:76-78`).                                                                                                                                                                                                                                                                                                | REAL logic; **legacy binding** ⚠️ | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 9   | `missing-native-script-tx` (6 steps)                | Native-script-locked input spent without the script in tx witnesses (`step-05.ak:66-69`, `step-06.ak:77-79`).                                                                                                                                                                                                                                         | REAL logic; **legacy binding** ⚠️ | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 10  | `no-reference-input` (4 steps)                      | Referenced input absent from pre-state and not produced in-block (`step-03.ak:70-76`, `step-04.ak:72-79`).                                                                                                                                                                                                                                            | REAL logic; **legacy binding** ⚠️ | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 11  | `withdrawn-reference-input` (3 steps)               | Referenced input was spent by a valid L2 withdrawal (`step-03.ak:75-92`).                                                                                                                                                                                                                                                                             | REAL logic; **legacy binding** ⚠️ | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🔶 Implemented, not fully verified — **binding blocker (§1a)**                              |
| 12  | `min-fee` (2 steps)                                 | Fee below network minimum. **STUB**: `get_min_transaction_fee` returns `0` (`min-fee/step-02.ak:78-80`), so the decisive `bad_tx_body_fee < 0` check (`:64`) is unsatisfiable — the proof can never finalize. Also on the **legacy binding** path.                                                                                                    | STUBBED; **legacy binding** ⚠️    | ❌                                       | ❌ none                                                                                                                                    | ❌                                                        | 🟠 Partial (inert) — **binding blocker (§1a)**                                              |
| 13  | `validation-trace` (interactive dispute)            | Canonical validation-claim dispute with opener/source/game/boundary/timeout/award controls and prepare, semantic, and direct resolvers.                                                                                                                                                                                                               | REAL control/resolver machinery   | ✅ `validationTraceDispute` (`00000006`) | ✅ manual `submit-validation-dispute-*` suite                                                                                              | ❌ full proof-to-removal acceptance not established       | 🔶 Implemented and catalogue-reachable; system acceptance remains open                      |

### 1a. Binding blocker — seven compiled proofs cannot bind to current counted-root native-v1 blocks

`double-spend`, `no-input`, `invalid-range`, and `zero-input` use the native binding path
`verify_native_tx_in_state_queue_node` (`common.ak:575-634`), which authenticates a raw
MPF root against the header's **counted** `transactions_root`
(`commit_counted_root(TransactionsRootDomain, raw_root, l2_transaction_count) ==
transactions_root`, `:615-620`) and then does `plutarch_phas_raw` over **raw native-tx
CBOR** (`:623-631`).

The other seven step-1 validators (types 5 and 7–12 above) still call the legacy
`verify_tx_in_state_queue_node` (`common.ak:518-573`), which (a) passes
`state_queue_datum.transactions_root` **directly** into `plutarch_phas` as the Merkle
root — with no counted-root unwrapping — and (b) matches over a **PlutusData
`MidgardTxCompact`** value, a different byte encoding than the raw native CBOR the block
actually commits. After PR #458 made `transactions_root` a counted/domain-tagged root,
this membership check cannot succeed against a real block. **Consequence:** these seven
types are not merely "untooled" — their inclusion binding is structurally unable to
match a committed native-v1 transaction until ported to the native counted-root path
(the port `no-input` already received). This is tracked as **W-C13** (widened) in
[`execution-plan.md`](execution-plan.md) and is a prerequisite for verifying any of
these families end-to-end. `invalid-range` and `no-input` prove the port is
mechanical; `double-spend` is the reference pattern.

## 2. Transition-trace fault families (`lib/midgard/fraud-proofs/transition-trace/proof.ak`)

| Family (redeemer)                                   | What it proves                                                                                                                                                                                                                                                                       | Impl | Tests (`proof.test.ak`)                                                       |
| --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---- | ----------------------------------------------------------------------------- |
| `TraceBoundaryFault` (`:43-46`, impl `:517-540`)    | First/last step doesn't connect to header `prev_utxos_root`/`utxos_root`                                                                                                                                                                                                             | REAL | ✅ pair                                                                       |
| `TraceLinkFault` (`:47`, impl `:542-554`)           | Adjacent steps don't chain                                                                                                                                                                                                                                                           | REAL | ✅ pair                                                                       |
| `EventToStepMismatch` (`:48-51`, impl `:569-598`)   | Step's event binding/phase wrong                                                                                                                                                                                                                                                     | REAL | ✅ pair                                                                       |
| `SourceMembershipMismatch` (`:52`, impl `:600-643`) | Sub-variants `MappedEventMissingFromSource`, `SourceEventMissingTrace`, `SourcePhaseMismatch` (`:108-127`)                                                                                                                                                                           | REAL | ✅ includes both phase-mismatch directions                                    |
| `InvalidOneStepTransition`                          | Withdrawal/deposit and invalid-forced direct transition faults remain unilateral. Accepted normal and valid-forced deltas are bound through the canonical V1 validation claim and terminal accepted-transition witness; source-phase mismatch covers both classification directions. | REAL | ✅ canonical V1 Aiken/TypeScript paths; concrete release measurements pending |
| `OmittedDueL1Event` (`:54`, impl `:1347-1431`)      | Due L1 event wrongly omitted from source root                                                                                                                                                                                                                                        | REAL | 🟠 deposit sub-variant only                                                   |
| `DuplicateTraceEvent` (`:55-58`, impl `:1527-1546`) | Two steps reference same event key                                                                                                                                                                                                                                                   | REAL | ✅ pair                                                                       |
| `OutOfWindowSourceEvent` (`:59`, impl `:1433-1525`) | Not-yet-due L1 event wrongly included                                                                                                                                                                                                                                                | REAL | 🟠 deposit sub-variant only                                                   |
| `CountFault` (`:60`, impl `:1548-1616`)             | Header/root count bookkeeping wrong (5 sub-variants)                                                                                                                                                                                                                                 | REAL | 🟠 1 of 5 sub-variants tested                                                 |

**Scope limits of the one-step L2 verifier** (`validate_l2_transaction_transition`,
`proof.ak:1117-1157`): it authenticates preimage hashes and MPF-replays the delete/insert
set against the ledger trie, then asserts root divergence. It does **not** check value
conservation, fee/mint correctness, or spend authorization — `witness_set_hash`,
`required_signers_hash`, `script_integrity_hash` are decoded but unused. Those checks are
(by design) the step-based proof types' job, which makes the missing value-family proofs
(§3) genuine coverage holes rather than redundancies.

**Valid forced execution**: canonical V1 replaced the historical rejection
branch with complete accepted-transition binding. A valid
forced source commits an accepted validation descriptor and terminal ledger
frontier; the accepted-transition proof compares that authenticated successor
with the transition trace's post-root. Concrete validator-hash-bound
publication, dispute, and settlement measurements remain release-gating
evidence rather than a disabled protocol branch.

## 3. Generic machinery status

| Component                                                                                     | Status | Notes                                                                                                                                                                                                                   |
| --------------------------------------------------------------------------------------------- | ------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Catalogue validator (`validators/fraud-proof-catalogue.ak`)                                   | ✅     | Mint = genesis-only w/ hub-oracle coupling; spend always fails (immutable root)                                                                                                                                         |
| Computation thread (`validators/computation-thread.ak`)                                       | ✅     | `Init` verifies catalogue membership + prover sig; `Success` trusts terminal step (`:130-139`); `BurnForCancellation` unencumbered                                                                                      |
| Fault-proof token (`validators/fraud-proof.ak`)                                               | ✅     | Permanent; requires 1:1 thread-burn/proof-mint (`:45-54`); no one-shot-UTxO uniqueness                                                                                                                                  |
| State-queue removal (`validators/state-queue.ak:524-712`)                                     | 🟠     | Works for same-operator chains; **cross-operator descendant check (`:661`) deadlocks cascades**; token is reference-input (reusable across removal txs) ✅                                                              |
| Operator slashing (`lib/midgard/operator-directory.ak:220-356`)                               | 🟠     | Wiring real (active/retired/already-slashed); **economics all zero**; penalty only enforced as `fee >= env.slashing_penalty`; bond-remainder routing to prover unenforced                                               |
| MPF primitives (`validators/phas.ak`, `pexcludes.ak`; Plutarch legacy in `onchain/plutarch/`) | ✅     | Aiken-native scripts are the deployed ones (env hashes match `plutus.json`); Plutarch package is legacy/parallel (`onchain/plutarch/README.md:1-8`). `plutarch_pdelete` unusable — env hash empty (`env/default.ak:68`) |
| Counted/domain-tagged roots (`lib/midgard/transition-trace.ak:64-80`)                         | ✅     | Landed via PR #458 (commit `5169b7f7`); consumed by settlement + no-input proof                                                                                                                                         |
| Native-tx CBOR codec (`lib/midgard/fraud-proofs/native-tx/`)                                  | ✅     | Real byte-offset decoders, hash-checked; vkey+timelock witnesses only (no Plutus)                                                                                                                                       |
| SDK catalogue deployment (`demo/midgard-sdk/src/fraud-proof/catalogue.ts`)                    | 🟠     | 7 of 13 top-level families; append-only IDs preserve `zeroInput` as `00000005` and append `validationTraceDispute` as `00000006`; single first-step hash per category; TODO for multi-step design (`common.ts`)         |

## 4. Ledger-rule helpers (on-chain)

| Helper                                                                                | Status                                                                              |
| ------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| L2 tx body parsing (`native-tx/` codec suite)                                         | ✅ real                                                                             |
| `verify_ed25519_signature` usage                                                      | ✅ real (duplicate-vkey TODO open)                                                  |
| Native-script hashing (`ledger_state.hash_midgard_script`)                            | ✅ presence/hash only — no interpreter for script _logic_ beyond timelock model     |
| Validity-interval helpers (`common/utils.ak:21-62`; `invalid-range/step-01.ak:20-44`) | ✅ real, unit-tested                                                                |
| Fee computation (`min-fee/step-02.ak:78-80`, TODO `:77`)                              | ❌ stub returns 0                                                                   |
| Min-ADA computation                                                                   | ❌ absent entirely (no `min_ada`/`coins_per_utxo` helper anywhere in `lib/midgard`) |
| Value conservation / mint arithmetic                                                  | ❌ absent (no proof type or helper sums values)                                     |
| MPF membership/non-membership delegation (`common/utils.ak:597-719`)                  | ✅ real                                                                             |

## 5. Delivery buckets (summary)

- **Delivered & functional (emulator-proven)**: generic machinery; double-spend, no-input,
  invalid-range, and zero-input full chains; transition-trace engine + library tooling.
- **Delivered, functional on-chain, but unreachable in deployment**: invalid-signature,
  missing-signature, missing-native-script-tx, no-reference-input,
  withdrawn-reference-input (real logic, not catalogue-registered, no tooling).
- **Delivered but inert**: min-fee (stub), slashing economics (zeroed),
  `plutarch_pdelete` (empty env hash).
- **Registered but untooled**: input-no-idx (`nonExistentInputNoIndex`).
- **Documented but missing / required but undocumented**: see §6 below for the proposed
  catalogue identifiers, and [`coverage-matrix.md`](coverage-matrix.md) for the
  rule-by-rule analysis.

## 6. Required-but-missing proof types — proposed catalogue identifiers

Proposed identifiers for every proof type the coverage analysis requires but that has no
compiled validator today. Naming follows the compiled types' kebab-case convention (§1).
These IDs are the reconciliation key between this file, the
[`coverage-matrix.md`](coverage-matrix.md) rows (§13 there), and the
[`execution-plan.md`](execution-plan.md) work items — addressing matrix §11 #2 (four
notions of "supported" that nothing reconciles). Register each in
`FAULT_PROOF_CATALOGUE_CATEGORY_ORDER` under this name when implemented.

| Proposed identifier           | Violation(s) / rule covered                                                                                                                                                                                                                                                                                                           | Work item                  | Decision gate               | Class                                                 |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------- | --------------------------- | ----------------------------------------------------- |
| `value-not-preserved`         | VALUE-NOT-PRESERVED                                                                                                                                                                                                                                                                                                                   | W-C2                       | D-C1                        | **fund theft**                                        |
| `ada-minted`                  | ADA-MINTED                                                                                                                                                                                                                                                                                                                            | W-C2                       | D-C1                        | **fund theft**                                        |
| `negative-output-value`       | NEGATIVE-OUTPUT-VALUE                                                                                                                                                                                                                                                                                                                 | W-C2                       | D-C1                        | **fund theft**                                        |
| `mint-authorization`          | non-ADA mint without satisfying policy                                                                                                                                                                                                                                                                                                | W-C2                       | D-S3                        | **fund theft**                                        |
| `min-ada`                     | MIN-ADA-TX / MIN-ADA-UTXO                                                                                                                                                                                                                                                                                                             | W-C5                       | D-S4                        | griefing                                              |
| `withdrawn-input`             | WITHDRAWN-INPUT (spend-side)                                                                                                                                                                                                                                                                                                          | W-C3                       | —                           | **fund theft**                                        |
| `double-withdraw`             | DOUBLE-WITHDRAW                                                                                                                                                                                                                                                                                                                       | W-C3                       | —                           | **fund theft**                                        |
| `req-signer-set`              | MISSING-REQ-SIGNER-TX/UTXO, NON-REQ-SIGNER                                                                                                                                                                                                                                                                                            | W-C3                       | D-C2 (vkey edge cases)      | **fund theft**                                        |
| `missing-native-script-utxo`  | MISSING-NATIVE-SCRIPT-UTXO                                                                                                                                                                                                                                                                                                            | W-C3                       | —                           | **fund theft**                                        |
| `native-script-invalid`       | NATIVE-SCRIPT-INVALID (on-chain timelock re-run)                                                                                                                                                                                                                                                                                      | W-C3                       | —                           | **fund theft**                                        |
| `reference-input-no-idx`      | REFERENCE-INPUT-NO-IDX                                                                                                                                                                                                                                                                                                                | W-C4                       | —                           | consistency                                           |
| `input-set-uniqueness`        | intra-tx duplicate spend/reference input; spend/reference disjointness (matrix §2/§3)                                                                                                                                                                                                                                                 | W-C4                       | —                           | **fund theft** once value proofs land                 |
| `network-id`                  | TRANSACTION-NETWORK, OUTPUT-NETWORK-TX/UTXO                                                                                                                                                                                                                                                                                           | W-C12                      | small design call           | consistency                                           |
| `output-well-formedness`      | malformed/undecodable output committed into `utxos_root`                                                                                                                                                                                                                                                                              | —                          | D-S10                       | griefing                                              |
| `hash-field-consistency`      | `auxiliary_data_hash` ≠ empty-null commitment; `script_integrity_hash` inconsistent (matrix §1)                                                                                                                                                                                                                                       | —                          | fold into D-S5 scope        | consistency                                           |
| `size-limits`                 | per-tx and per-block size/count bounds (matrix §11b)                                                                                                                                                                                                                                                                                  | —                          | D-S1                        | provability prerequisite                              |
| `fabricated-deposit`          | deposit event without L1 UTxO, or content misstating it                                                                                                                                                                                                                                                                               | W-C7                       | D-S7                        | **fund theft**                                        |
| `fabricated-withdrawal`       | withdrawal event without/misstating the L1 order                                                                                                                                                                                                                                                                                      | W-C7                       | D-S7                        | **fund theft**                                        |
| `withdrawal-mistag`           | valid↔invalid mis-tagging incl. `UnpayableWithdrawalValue`                                                                                                                                                                                                                                                                           | W-C7                       | D-S8                        | censorship / **fund theft**                           |
| `cross-block-duplicate-event` | same L1 event applied in two blocks; evidence must survive event-NFT burn (matrix §7)                                                                                                                                                                                                                                                 | —                          | D-S11                       | **fund theft**                                        |
| `valid-forced-transition`     | Implemented in canonical V1 through the generic accepted validation claim plus forced-source binding; retained here as a release-evidence matrix identifier, not a missing catalogue type.                                                                                                                                            | complete; evidence pending | D-S9 resolved in source     | censorship                                            |
| `l2-tx-mistag`                | operator marks a **valid L2 tx** as invalid (`validity_code != 0`) so it applies as a no-op — the L2 analogue of `withdrawal-mistag`; there is no `InvalidL2TransactionNoOpTransition` constructor (only withdrawals/forced txs have the invalid-no-op form, `proof.ak:156-177`), so a valid L2 tx dropped this way is caught nowhere | W-C14                      | D-S9 (same censorship gate) | censorship                                            |
| `da-hash-preimage`            | DA payload hash/preimage mismatch (`6-transaction.tex:175`)                                                                                                                                                                                                                                                                           | —                          | D-DA1                       | provability                                           |
| `script-failure`              | Plutus/MidgardV1 execution disputes (no committed CEK trace today)                                                                                                                                                                                                                                                                    | —                          | D-S5                        | **fund theft** or documented launch-scope restriction |

**Confirmed independently addable (second-pass verification).** The value family
(`value-not-preserved`, `ada-minted`, `negative-output-value`, `mint-authorization`) and
`min-ada` are **not** blocked by any binding gap: the native inclusion path
(`verify_native_tx_in_state_queue_node`, `common.ak:575-634`) already binds a new step
proof to any L2 tx in a committed block's `transactions_root`, and each spent input's
value is available exactly as `apply_l2_spends` fetches it (ledger-membership witnesses
carrying `witness.value`, `proof.ak:680-692`). The transition-trace one-step verifier
checks none of value/mint/fee/authorization/min-ada/network-id per step
(`proof.ak:1117-1157`), and a self-consistent trace that faithfully applies an _invalid_
tx (`validity_code==0`, outputs > inputs) produces a correct root — so it is **not** a
trace fault and **must** be an independent step proof (W-C14), built on
`pass_native_tx_to_next_step` (not the legacy path — see §1a).

Gaps that are **fixes, not catalogue entries** (no new proof type):

- **D-S12** — commit `end_time` bound (commit-time check).
- **W-C13** — port the seven remaining legacy-binding proofs to the native counted-root path
  (§1a); subsumes the witness-set encoding split.
- **W-C9** — descendant-removal semantics.
- **W-C15 / D-DA4** — DA committee rotation is not retroactive: an attestation that
  reached quorum under an old committee stays applicable after governance rotates the
  committee out, because `ApplyToStateQueue` re-reads neither the committee nor the
  threshold (`da-attestation.ak:336-376`, `state-queue.ak:350-352`). Fix: re-assert the
  current governed `committee_signers_hash`/`da_threshold` at apply; also re-derive
  `blake2b_256(committee)` inside `get_da_params` (`da-attestation.ak:69-91`).
- **D-L1** — liveness kill-switch cluster: (i) an unsigned but non-faulty block can
  **never** be merged (merge unconditionally requires the DA attestation,
  `state-queue.ak:350-352`) and can't be removed (it isn't faulty), permanently halting
  the head-of-line queue; (ii) the escape hatch is a bare TODO (`state-queue.ak:716`),
  so users have no force-exit when operators stall — yet the censorship invariant
  depends on it (`1-protocol-invariants.tex:65`). Needs a merge-timeout/fallback and the
  escape-hatch validator.
- **D-DA5** — DA-params governor lets an owner quorum drop `da_threshold`/`update_threshold`
  to 1 with no lower bound and no drain-protection on the owner set itself
  (`da-params-governor.ak:98-127,215-222,287-328`) — DA capture + governance takeover.
  A mid-flight committee change also bricks partially-signed attestations and strands
  their ADA (`da-attestation.ak:143-144,261,348-349`). Decide governed lower bounds and
  an attestation-rescue/refund path.
- **D-S13 resolved for canonical V1** — the exact consensus tuple and tuple
  digest bind protocol version 1, transition schema 1, native transaction
  version 1, protocol-info API 1, and `DeploymentManifestV1`. Unknown
  identities fail closed; a future upgrade requires a new deployment.
- **D-DA2** — data withheld after attestation (remedy, not proof).
- **D-E1 / W-C10** — economics.
- **Genesis/deployment trust (record, no runtime fix)**: the hub-oracle datum — every
  downstream policy id and address — is unvalidated at mint and then immutable (no spend
  handler), so a wrong genesis datum is unfixable (`hub-oracle.ak:6-27`); and the
  catalogue must register **first-step** hashes only, else `Init` could jump mid-chain
  (`computation-thread.ak:42-61`). Both are deployment-checklist items.
- **Machinery hardening (record + test)**: the computation-thread mint policy is a
  "blank check" (`BurnForCancellation` has no auth of its own, `computation-thread.ak:140-149`)
  whose safety is delegated to every step's `Cancel` calling `common.cancel`
  (prover-sig gate, `common.ak:292-293`) — add a catalogue-immutability/all-steps-guard
  test so a future step that forgets it can't make threads burnable by anyone.

Verified non-gaps from the same sweep (recorded in matrix §11c): deposits have **no
refund path** (`deposit.ak` spend is absorb-to-reserve only), so an included deposit
cannot also be reclaimed on L1; tx-order and withdrawal refunds are settlement-gated
(`tx-order.ak:93-97`, `withdrawal.ak:326-349`) and adjudicated by the trace proofs;
reserve→payout→conclude conservation is airtight per-transaction (`payout.ak:271-373`,
`withdrawal.ak:294-321`) — the only residual is that **no L1 check sums aggregate
reserve backing** (the standard optimistic assumption, matrix §14); `header_hash`
transitively binds every root (linked-list key = `blake2b_224(serialise_data(header))`),
so signature-only attestation is **not** a root-integrity hole and must not be
"hardened" with redundant per-root checks.
