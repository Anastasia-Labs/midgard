# Withdrawn-reference-input fault: offchain implementation plan (v1)

> **Registration update (2026-08-26):** this family is now registered as
> `withdrawnReferenceInput` at `00000010`. Generic Init,
> catalogue/inspection, node/core deployment identity, watcher proof-thread
> topology, and all three mandatory authenticated reference scripts are wired.
> Family-specific CLI, autonomous watcher detector/prover mounting, preprod,
> and live evidence remain open. The identity change requires fresh
> genesis/redeployment; there is no migration or compatibility path.

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`).
Task: **Q19** (`GOAL_SPEC.md` §9.3), REFERENCE side only. Catalogue row:
`catalogue-status.md` §1 row 11. Unlike the Q16 missing-signature document,
this is a plan **and** the wave that executes it: the modules, harness
extensions, and emulator suites this plan specifies land uncommitted in the
same working tree as this document. The spend-side sibling
(`withdrawn-input`, row 10) is a separate wave and is out of scope here
(§11).

The parity bar is the `native-script-decoding` family (row 18) as planned in
`native-script-decoding-offchain-plan-v1.md` and built on this branch: a
per-family module directory with explicit parent-owned contracts records,
per-step submitters plus init and cancel, canonical catalogue harness wiring,
and lucid-evolution emulator
suites in both polarities — through fraud-proof mint **and**
fraudulent-commitment removal. Everything that family's plan decided
generically is inherited here, not re-decided.

Standing rulings this plan implements and never re-opens:

- **Reference scripts always (owner ruling 2026-08-26):** fault-proof
  step validators deploy as reference scripts and are referenced, never
  attached inline, regardless of compiled size (§2.3, §10 D2).
- **Both-polarity emulator tests (owner directive 2026-08-25):** every
  offchain contract plan includes lucid-evolution emulator tests of
  realistic scenarios in both polarities — the real fault proves through
  the full lifecycle, and an adversarial prover against an honest
  commitment is refused **on-chain at the exact check**, not merely by
  offchain guards.
- **Explicit-record discipline:** contracts remain explicit while
  `withdrawnReferenceInput` is canonically routed through catalogue and
  deployment manifests. Family-specific CLI remains separate.
- **Canonical id:** `00000010` is the production category id.
- **Removal via explicit category:** `remove-fraudulent-block.ts`'s
  `RemoveFraudulentBlockExplicitCategory` seam lets the
  family drive removal with every fail-closed check intact and **zero**
  further changes to that module. The fraud-proof token is permanent by
  design (the state-queue node NFT burns; the token survives as evidence
  and as the `alreadyProven` gate).
- **Cancellation is an explicit prover decision** (decoding
  `submit-native-script-decoding-cancel.ts` policy): builders never cancel
  on their own; a dedicated cancel submitter exists for the prover.

All `file:line` anchors are against HEAD `a1724e63`.

---

## 1. The contract the builders must satisfy

The as-built onchain family
(`onchain/aiken/validators/fraud-proofs/withdrawn-reference-input/step-0{1..3}.ak`,
wire twins under
`onchain/aiken/lib/midgard/fraud-proofs/withdrawn-reference-input/`) is REAL
and complete: it proves that a committed transaction references an input
that the **same block's** counted withdrawals root commits a **valid** L2
withdrawal for. Three steps, three validators:

| Step | Blueprint title | Params (positional) | Decides |
| --- | --- | --- | --- |
| 01 | `fraud_proofs/withdrawn_reference_input/step_01.main.spend` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle` | Binds the disputed tx (`pass_native_tx_to_next_step`, `common.ak:575-634`); forwards `{bad_tx_id, blocks_withdrawals_root: header.withdrawals_root, blocks_withdrawal_count: header.withdrawal_count}` |
| 02 | `fraud_proofs/withdrawn_reference_input/step_02.main.spend` | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id` | Opens the tx's field-1 (reference inputs) preimage through the §8.8 door (`opened_field_view` + `BodyAnchor`), selects `missing_reference_input = spend_input_at(view, bad_reference_input_index)` (the fixed-stride reader serves fields 0 and 1 identically; abort-never-clamp) |
| 03 | `fraud_proofs/withdrawn_reference_input/step_03.main.spend` | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id` | `common.finalize`, then the decisive checks (`step-03.ak:75-98`) |

Step-03's decisive checks, verbatim from the validator:

1. `withdrawal_membership.value` must destructure as
   `WithdrawalInfo { body: WithdrawalBody { l2_outref, .. }, validity: WithdrawalIsValid, .. }`
   — an **invalid withdrawal never convicts**: any other `validity`
   constructor fails the `expect` before membership is even considered.
2. `l2_outref.transaction_id == missing_reference_input.tx_id` and
   `l2_outref.output_index == missing_reference_input.output_index` — the
   committed withdrawal must spend exactly the UTxO the disputed
   transaction references.
3. `transition_trace.verify_root_membership_with_bytes(withdrawal_membership,
   WithdrawalsRootDomain, blocks_withdrawals_root, blocks_withdrawal_count,
   cbor.serialise(withdrawal_membership.key),
   cbor.serialise(withdrawal_membership.value))` — domain/root/count
   equality, `counted_root_is_consistent`, and raw MPF membership of the
   **aiken-canonically serialised** key/value pair under the header's
   counted withdrawals root.

The wire types the offchain schemas must mirror positionally
(`lib/midgard/fraud-proofs/withdrawn-reference-input/step-0{1..3}.ak`):

- step-01 `Args` = bare `NativeTxInclusionArgs` (no carriage wrapper —
  this family predates the #545 carriage sum and stays on the
  redeemer-carried route; §5).
- step-02 `State { bad_tx_id, blocks_withdrawals_root, blocks_withdrawal_count }`;
  `Args { input_index, output_index, reference_inputs_opening: FieldOpeningV1, bad_reference_input_index }`.
- step-03 `State { missing_reference_input: MidgardTxInput, blocks_withdrawals_root, blocks_withdrawal_count }`;
  `Args { input_index, output_index, fraud_proof_mint_redeemer_index, withdrawal_membership: RootMembershipProof<WithdrawalId, WithdrawalInfo> }`
  (`RootMembershipProof` field order: `domain, root, phas_root, count, key,
  value, proof` — `transition-trace.ak`).

The onchain selector inventory is the adversarial-suite spec: step-01 has
the positive binding selector; step-02 has
forwards-the-challenged-reference-input plus two `fail` selectors
(substituted preimage, out-of-range index); step-03 has
accepts-committed-withdrawal plus the valid-block `fail` selector (root
commits a withdrawal of a **different** UTxO). Every `fail` selector maps
to an emulator adversarial scenario in §8.2.

Serialisation identity that everything downstream leans on: step-03 hashes
`cbor.serialise(key)`/`cbor.serialise(value)`, so the offchain MPF leaves
**must** be inserted with aiken-canonical bytes —
`committedWithdrawalKeyBytesV1(withdrawalId)` /
`committedWithdrawalValueBytesV1(info)` from
`demo/midgard-sdk/src/fraud-proof/fabricated-withdrawal-v1.ts`
(`aikenSerialisedPlutusDataCbor` over `Data.to`). Node-canonical CBOR of
the same Plutus data is NOT byte-identical in general; using it would build
roots the validator can never verify.

## 2. Registration

### 2.1 Category id

Canonical category id: **`00000010`** (`categoryId(16)`, 4-byte BE hex).
The SDK catalogue, generic Init, deployment manifests/inspection, and watcher
proof-thread topology bind it to the applied step-01 hash.

### 2.2 Registered deployment surface

`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, generic `submit-init.ts`, canonical
deployment manifests, inspection, and watcher proof-thread topology are wired.
Family-specific `bin.ts` verbs and autonomous watcher detector/prover mounting
remain open.

### 2.3 Script deployment: reference scripts (owner ruling 2026-08-26)

All three step validators deploy as **reference scripts** and are consumed
via `readFrom`, never attached inline. Compiled sizes at the fresh
patched-fork blueprint (aiken v1.1.23+2a78108, 452 validators): step-01
5804 B, step-02 7116 B, step-03 5631 B — all comfortably publishable as
single reference-script UTxOs (`publishPlainReferenceScriptUtxo` with
`oversized: true` chunk handling available but not needed). The emulator
suites publish per-step reference scripts before driving the thread, same
as the decoding suites.

## 3. Detection and evidence

### 3.1 The fault, precisely

A block header `H` commits (a) a transactions root containing native tx `T`
whose body field 1 (reference inputs) includes outref `R`, and (b) a
counted withdrawals root containing a withdrawal event whose
`WithdrawalInfo.body.l2_outref == R` with `validity == WithdrawalIsValid`.
The block is internally inconsistent: a valid withdrawal consumed `R`, so
no transaction in the same committed ledger state may reference it.

### 3.2 Classification boundaries (which family convicts)

- **Same-block conflict — THIS family.** Step-01 forwards *this header's
  own* `withdrawals_root`/`withdrawal_count`; step-03's membership proof is
  against that root. The tx and the withdrawal are committed under the same
  header.
- **Withdrawal in an EARLIER block:** by the time `H` is committed, `R` is
  absent from `H`'s previous-UTxO set — that is the `no-reference-input`
  family's domain (membership against `prev_utxos_root`), not this one.
- **Withdrawal in a LATER block:** not a fault of `T` at all — `R` was
  live when `H` committed. Classification must refuse to route this here.
- **Invalid withdrawal (`validity != WithdrawalIsValid`):** never a
  conviction. An invalid withdrawal did not consume anything; step-03's
  first `expect` enforces this on-chain, and the prepare module refuses it
  fail-closed offchain (§4.3) so a prover never opens a doomed thread.

### 3.3 Evidence record (per step)

The prepare module (§4.3) assembles one immutable evidence record from a
committed header + the block's transaction set + the block's withdrawal
set:

- **Step-01:** `SubmitStep01TxInclusion` (reused verbatim from
  `submit-step-01.ts`): `nativeTxId`, `nativeTx`, `nativeTxCompactCbor`,
  `transactionsPhasRoot`, `txMembershipProof(+Cbor)`. Authenticated
  offchain by recomputing
  `commitCountedRootProgram({domain: ROOT_DOMAINS.transactionsV1, phasRoot,
  count: header.l2TransactionCount})` against `header.transactionsRoot`
  before submission (fail-closed, same as
  `submit-no-reference-input-step-01.ts`).
- **Step-02:** the decoded reference-inputs list (as `MidgardTxInput[]`),
  their canonical item CBORs (`encodeMidgardTxInputCanonicalV1`), and the
  accused `badReferenceInputIndex` — the index of the first (or a caller-
  selected) reference input matched by a committed valid withdrawal.
- **Step-03:** the full `WithdrawalSourceMembershipProof`
  (`demo/midgard-sdk/src/transition-trace.ts`): `{domain: withdrawals,
  root: header.withdrawalsRoot, phas_root, count: header.withdrawalCount,
  key: WithdrawalId (OutputReference), value: WithdrawalInfo, proof}` with
  the MPF proof generated over `committedWithdrawalKeyBytesV1`/
  `committedWithdrawalValueBytesV1` leaves.

### 3.4 Offchain pre-verification (fail-closed twins of the on-chain checks)

Before any submission, prepare verifies locally: counted-root equality for
both roots; `validity === "WithdrawalIsValid"`; `l2_outref` equality with
the accused reference input; and local MPF verification of the withdrawal
leaf under `phas_root`. Any failure returns a typed refusal (no thread is
opened, no funds are risked). These are conveniences, not the security
boundary — the validator re-checks everything.

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk/src/fraud-proof/withdrawn-reference-input-v1.ts`)

Mirrors `native-script-decoding-v1.ts` in shape and
`no-reference-input.ts` in content:

- `WITHDRAWN_REFERENCE_INPUT_VIOLATION_V1` violation-id constant.
- `withdrawnReferenceInputThreadTokenAssetNameV1(categoryId, headerHash)`
  with the 8/56-hex regex validation.
- Step-01: `Datum/SpendRedeemer` over the bare `NativeTxInclusionArgsSchema`.
- Step-02: `State {bad_tx_id: H32, blocks_withdrawals_root: H32,
  blocks_withdrawal_count: Integer}`, `Args {input_index, output_index,
  reference_inputs_opening: FieldOpeningV1Schema,
  bad_reference_input_index}`.
- Step-03: `State {missing_reference_input: MidgardTxInputSchema,
  blocks_withdrawals_root, blocks_withdrawal_count}`, `Args {input_index,
  output_index, fraud_proof_mint_redeemer_index, withdrawal_membership:
  WithdrawalSourceMembershipProofSchema}` — the withdrawal membership
  schema already exists in `transition-trace.ts` and is reused, not
  redefined.
- Exhaustive step-datum resolver (decoding style).
- Barrel line in `fraud-proof/index.ts` (alphabetical, before
  `zero-input.js`).

### 4.2 Family modules (`demo/midgard-fault-proofs/src/withdrawn-reference-input/`)

Decoding-style directory (module organization is independent of registration):

| Module | Role |
| --- | --- |
| `contracts-v1.ts` | `BLUEPRINT_TITLES`, `WithdrawnReferenceInputContractsV1` explicit record (steps tuple + computationThread + fraudProof + hubOracle/stateQueue/fieldPreimageCertificate policy ids), param-order docblock; no categoryId field |
| `submit-common-v1.ts` | category record type, thread-UTxO/reference-script/step-state fail-closed helpers (decoding shape) |
| `prepare-withdrawn-reference-input-v1.ts` | evidence builder + classification (§3.3/§3.4) |
| `submit-withdrawn-reference-input-init.ts` | Init mint under an explicit catalogue category (PHAS catalogue membership withdrawal), first-step datum `{fraud_prover, data: null}` |
| `submit-withdrawn-reference-input-step-01.ts` | tx binding via the PHAS transactions-membership withdrawal (bare-args Continue), counted-root cross-check, next datum per §1 |
| `submit-withdrawn-reference-input-step-02.ts` | tier-1 inline `FieldOpeningV1` via `planFaultProofFieldOpeningV1({fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs, ...})` + bounds pre-check |
| `submit-withdrawn-reference-input-step-03.ts` | finalize: thread burn `Success`, fraud-proof token mint, pay-to-`fraudProof.spendingScriptAddress` with inline `FraudProofTokenDatum`, plus the `withdrawal_membership` Continue arg and §3.4 pre-checks |
| `submit-withdrawn-reference-input-cancel.ts` | prover-only Cancel at any step (decoding shape) |
| `index.ts` | barrel; re-exported from `src/index.ts` |

### 4.3 The proving core

For this wave the "core" is the prepare module: given `{header, blockTxs,
withdrawals, accusedTxId?}` it classifies (§3.2), refuses non-faults with
typed reasons (`withdrawal-not-valid`, `no-matching-reference-input`,
`wrong-block-ordering` is unobservable from a single block and therefore
out of scope of classification here — callers supply same-block data by
construction), and emits the three-step evidence record. Watcher/CLI
adapters remain operational work (§2.2), same as decoding; topology
registration alone does not mount them.

## 5. Carriage frontiers

- **Step-01** stays on the bare redeemer-carried `NativeTxInclusionArgs`
  (the validator's wire type has no carriage sum). Oversized compact CBOR
  is bounded by the same envelope discipline as `no-reference-input`; no
  published-chunk route exists for this family until the onchain type
  grows one. Not this wave's problem to solve; recorded as frontier F1.
- **Step-02** uses tier-1 inline `FieldOpeningV1` carriage
  (`faultProofFieldOpeningV1({planned, label})`) — reference-inputs
  preimages are small (fixed-stride items). Certificate-published carriage
  remains available through the same planner if a pathological tx demands
  it; the submitter takes the planner's verdict.
- **Step-03** carries `withdrawal_membership` wholly in the redeemer.
  `WithdrawalInfo` embeds an L1 `Address`, a `Value`, and a datum, so a
  pathological withdrawal could bloat the redeemer; for realistic
  withdrawals this is far under limits. **Frontier F2 (escalation
  clause):** if a real-world withdrawal's serialised `WithdrawalInfo`
  pushes the finalize tx over budget, that needs an onchain carriage arm
  (owner decision), not an offchain workaround. Recorded, not blocking.

## 6. Economics and pacing

Identical profile to `no-reference-input` plus one leaf proof: 4 txs
(init + 3 steps) with `MIN_FEE_INPUT_LOVELACE` fee-input selection, thread
token minted once and burned at finalize, fraud-proof token minted
permanently. No new economics decisions.

## 7. Cancel, recovery, and the corners

### 7.1 Crash-resume

Threads are resumable mid-flight: each submitter locates the thread UTxO by
address + thread-token asset name and fail-closed reads the step state, so
a prover who crashed after step-01 re-runs step-02 directly. The negatives
suite exercises resume-after-interrupt.

### 7.2 Corner semantics (normative for tests)

1. **Invalid withdrawal must NOT convict** — on-chain `expect
   WithdrawalIsValid` (§1 check 1); offchain classification refusal
   (§3.4). Tested in both planes.
2. **Withdrawal in a later block** is not this fault (§3.2); same-block
   evidence construction makes it unrepresentable in prepare, and the plan
   records the routing rule for the watcher wave.
3. **Withdrawal in an earlier block** routes to `no-reference-input`.
4. **Different-outref commitment (honest block):** two adversarial roads,
   both refused on-chain — a genuine leaf for `R′` fails the `l2_outref`
   equality expects; a forged claimed-`R` value over the `R′` trie fails
   `verify_root_membership_with_bytes` because the serialised value bytes
   diverge.
5. **Substituted field-1 preimage / out-of-range index** at step-02 —
   door authenticate-once refusal / abort-never-clamp.
6. **Outsider interference:** only the fraud prover's signature drives or
   cancels the thread.

## 8. Testing

### 8.1 Where and how

Lucid-evolution emulator, one suite per file (the `@lucid-evolution/uplc`
wasm heap is never reclaimed within a worker — one-file-per-suite is the
standing rule). `localUPLCEval: true` so validator aborts surface at
`.complete()`. On-chain refusals asserted via `expectOnchainRefusalV1`
(message must match `/failed script execution/`); offchain refusals via the
submitters' typed error message regexes. Raw guard-bypassing builders
(decoding's `submitRawDecodingStepV1` pattern) deliver adversarial
redeemers past the honest submitters. The #610 gate holds: no
`compiledCode` reads outside the envelope suite.

### 8.2 Suites

1. `tests/submit-init-emulator-withdrawn-reference-input-lifecycle.test.ts`
   — real-fault polarity: scenario (§8.3) → publish reference scripts →
   init → step-01 → step-02 → step-03 → assert thread token burned,
   fraud-proof token minted at `fraudProof.spendingScriptAddress` with
   `FraudProofTokenDatum{fraud_prover}` → removal leg
   (`submitRemoveFraudulentBlock` with the explicit category record;
   assert remove-target/slashing/operator-removed/state-queue-emptied/
   scheduler-`NoActiveOperators`) → fraud-proof token retained at the same
   out-ref → second removal refused
   (`/State queue does not contain block/`).
2. `...-adversarial.test.ts` — honest-commitment scenarios refused at the
   exact check: different-outref roads (both), invalid-withdrawal leaf,
   substituted step-02 preimage, out-of-range index; offchain-plane twins
   (prepare/submit refusals) alongside each on-chain refusal.
3. `...-negatives.test.ts` — cancel at each step by the prover, thread
   restart after cancel, resume mid-thread, outsider cannot drive or
   cancel.

### 8.3 Scenario construction (shared support module)

`tests/support/withdrawn-reference-input-emulator-v1.ts` builds the
committed conflict:

- Withdrawn outref `R`; native tx with
  `referenceInputsPreimageCbor = encodeCbor([encodeMidgardTxInputCanonicalV1(R)])`
  (decoding subject-tx recipe otherwise unchanged);
  `countedTx = buildCountedRoot(transactionsV1, [[txId, compactCbor]])`
  and `keyValuePhasProof` for the `SubmitStep01TxInclusion`.
- Withdrawal: arbitrary `WithdrawalId` (OutputReference);
  `WithdrawalInfo {body: {l2_outref: R, l2_owner, l2_value: empty,
  l1_address, l1_datum}, signature: [vkey, sig], validity:
  "WithdrawalIsValid"}`; leaf bytes via
  `committedWithdrawalKeyBytesV1`/`committedWithdrawalValueBytesV1`;
  `countedW = buildCountedRoot(withdrawals, [leaf])` + membership proof
  over `countedW.phasRoot`.
- Header:
  `{...makeHeader(funderKeyHash, startTime, countedTx.root, 1n),
  withdrawalsRoot: countedW.root, withdrawalCount: 1n, totalEventCount:
  2n, transitionStepCount: 2n}` — `validationTraceCount` stays 1 (l2 tx
  only; withdrawals contribute to events, not traces), satisfying every
  `header_transition_commitments_v1_are_valid` identity
  (`ledger-state.ak:215-298`); `root_matches_count_v1` accepts the
  placeholder non-empty trace roots supplied by `makeHeader`.
- `submitSetupTx` commits the header through the standing 4-tx journey.

### 8.4 Test-support extensions (extend, do not fork)

- `tests/support/emulator/contracts.ts`:
  `buildWithdrawnReferenceInputChainV1` (parameterise step-03 first —
  `[fraudProofPolicyId, fraudProofTokenAddressData,
  computationThreadPolicyId]` — then step-02, then step-01) + a
  `realWithdrawnReferenceInput` conditional record sharing the base
  computation-thread/fraud-proof contracts, mirroring
  `realNativeScriptDecoding`.
- `tests/support/emulator/harness.ts`:
  `WITHDRAWN_REFERENCE_INPUT_TEST_CATEGORY_ID_V1 = "00000010"` + a fourth
  extra-categories spread keyed `withdrawnReferenceInput` (scriptHash =
  step-01's).
- `tests/support/emulator/removal-deployment.ts`:
  `WITHDRAWN_REFERENCE_INPUT_REMOVAL_DEPLOYMENT_ENTRY_V1 =
  "fraudProofWithdrawnReferenceInput"` + conditional entry spread
  (the canonical manifest now carries the production entry too).

## 9. Sequencing

1. SDK schemas (§4.1) — everything downstream imports them.
2. Family modules (§4.2).
3. Test-support extensions (§8.4) + scenario module (§8.3).
4. The three suites (§8.2), lifecycle first.
5. Catalogue-status row 11 update (this row only, minimal diff).

No onchain changes. Blueprint already built with the patched fork.

## 10. Decision register

| # | Decision | Rationale |
| --- | --- | --- |
| D1 | Canonical category id `00000010` = `categoryId(16)` | Production catalogue/deployment identity (§2.1) |
| D2 | Reference scripts for all three steps | Owner ruling 2026-08-26; sizes in §2.3 are informational only |
| D3 | Same-block corner semantics (§3.2/§7.2): later-block never routes here; earlier-block routes to `no-reference-input`; invalid withdrawal refused in both planes | Matches the validator's decisive checks exactly; most canonical-V1-consistent reading of §5.1.16 |
| D4 | Step-03 `withdrawal_membership` rides the redeemer; frontier F2 escalation clause for pathological `WithdrawalInfo` sizes | The onchain wire type admits nothing else; realistic sizes are small; growing a carriage arm is an owner decision |
| D5 | Decoding-style module directory, not flat legacy files | Module organization is not a registration signal |
| D6 | Plan + implementation land in one wave (unlike Q16's planning-only doc) | On-chain side is already REAL and complete; nothing blocks execution |
| D7 | MPF leaves strictly via `committedWithdrawalKeyBytesV1`/`ValueBytesV1` | `cbor.serialise` parity (§1); node-canonical CBOR would build unverifiable roots |
| D8 | `expectOnchainRefusalV1` imported from the decoding support module rather than hoisted | Smallest diff now; the Q16 plan's D7 already records the eventual hoist |

## 11. Out of scope

- The spend-side `withdrawn-input` sibling (row 10) — separate parallel
  wave; nothing here designs or touches it.
- Family-specific CLI verbs and autonomous watcher adapters (§2.2).
- Onchain changes of any kind.
- Cross-block routing implementation (recorded as classification rules
  only; the watcher wave owns routing).
