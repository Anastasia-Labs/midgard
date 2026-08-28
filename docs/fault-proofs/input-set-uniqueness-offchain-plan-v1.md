# Input-set-uniqueness fault: offchain implementation plan (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`) plus
the uncommitted 2026-08-26 working tree this plan is authored alongside:
unlike the missing-signature plan (planning-only), this document and the
family it specifies land in the same working tree, so §1–§8 describe the
as-built contract and record why it is shaped that way. Task: the **W-C14
single-party conversion** of the validation machine's InputSets rule
(`reject_duplicate_input` — MACHINE-COVERED in `catalogue-status.md` §6).
This plan registers nothing and deploys nothing.

The parity bar is the `native-script-decoding` family as planned in
`native-script-decoding-offchain-plan-v1.md` and built on this branch, with
the missing-signature plan's structure as the documentary template.
Everything those plans decided generically — consumer-agnostic submitters,
pre-registration discipline, emulator harness shape, removal seam — is
inherited here, not re-decided.

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
- **Pre-registration explicit-record discipline:** pre-registration
  families must not route their ids through the deployment manifest —
  `parseFraudProofCatalogueDeploymentInfo` silently drops non-canonical
  keys. Contracts records are explicit and parent-owned; the SDK
  catalogue order, `submit-init.ts`'s category union, and `bin.ts` are
  untouched until the registration wave.
- **Removal via explicit category:**
  `remove-fraudulent-block.ts`'s `RemoveFraudulentBlockExplicitCategory`
  seam drives removal for a pre-registration family with every
  fail-closed check intact and zero changes to that module. The
  fraud-proof token is permanent by design (the state-queue node NFT
  burns; the token survives as evidence and as the `alreadyProven` gate).
- **Cancellation is an explicit prover decision:** submitters never
  cancel on their own; `ct.Cancel` exists on both steps for the prover.

All `file:line`-level references are against this working tree.

---

## 1. The contract the builders must satisfy

The onchain family
(`onchain/aiken/validators/fraud-proofs/input-set-uniqueness/step-0{1,2}.ak`,
lib wire twins `lib/midgard/fraud-proofs/input-set-uniqueness/step-0{1,2}.ak`)
is the byte-for-byte target. The fault statement: a transaction the
operator committed as **accepted** (`validity_code == 0`) violates the
intra-transaction input-set rules — §2.5 field 0 (spend inputs) contains a
duplicate out-ref, field 1 (reference inputs) contains a duplicate
out-ref, or the two fields are non-disjoint. Strictly INTRA-transaction:
empty spend-input sets belong to the zero-input family, and cross-block
double spends to the double-spend family (§11).

**Step chain** `Init → 01 → 02`, `ct.Cancel` on both steps — **two steps,
three L1 transactions** plus removal. Step count against precedent (§10
D1): the zero-input family proves its predicate in 2 steps because the
bound compact structure alone decides it; input-no-idx needs 4 because it
must anchor, select, and then adjudicate an item against L1 state. This
fault sits between: it needs field *openings* (the compact structure
commits only field hashes) but no external state, no vkey lift, no
per-item follow-on anchoring — two items of one or two fields of the
**same** bound anchor, compared for byte equality. That fits entirely in
the finalize step behind the §8.8 door, so any third step would carry
state and burn fees while deciding nothing.

Parameterization (acyclic, applied backwards, step-02 first):

| Validator | Parameters (blueprint-declared order) | Compiled |
|---|---|---|
| `fraud_proofs/input_set_uniqueness/step_01` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle` | 6,487 B |
| `…/step_02` | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id` | 7,571 B |

**Step-01** (acceptance-evidence binding — soundness guard): the shared
native binding via `pass_native_tx_to_next_step_carried` — counted
`transactions_root` authentication off the state-queue node (read through
the hub oracle) plus PHAS MPF membership over the raw compact CBOR, with
both carriage arms (`RedeemerCarriedInclusion` and the #545
`PublishedChunkInclusion`). Init binds only this first step; every later
hop is thread-carried. On top of the shared binding the step enforces the
family's acceptance gate: `expect bad_tx_view.tx_compact.validity_code ==
0` — a transaction the operator honestly recorded as a no-op must never
convict, however degenerate its input sets (§8 both suites pin this).
Output at step-02's address: `step_02.State { bad_tx_id }` — the §2.5
anchor rides the thread; no redeemer downstream can restate it.

**Step-02** (conviction + finalize): three `Continue` arms, constructor
order = wire order:

- `DuplicateSpendInputs { input_index, output_index,
  fraud_proof_mint_redeemer_index, first_index, second_index,
  spend_inputs_opening: FieldOpeningV1 }` — opens field 0 through the
  §8.8 door (`opened_field_view`, `BodyAnchor { tx_id: bad_tx_id }`),
  `expect first_index < second_index`, then byte equality of
  `field_item_at(first_index)` and `field_item_at(second_index)`.
- `DuplicateReferenceInputs { …, reference_inputs_opening }` — the same
  predicate over field 1.
- `SpendReferenceOverlap { …, spend_index, reference_index,
  native_tx_compact_cbor, spend_inputs_carriage: FieldCarriageV1,
  reference_inputs_carriage: FieldCarriageV1 }` — anchors the compact
  structure once (`anchored_native_tx`), opens **both** fields against it
  (`anchored_field_view` twice), and compares one item of each. **No
  index relation**: the same position in two different lists is only a
  fault when the out-refs match (§7 corner c).

All indices name §5.3 **items**; the door derives byte positions
arithmetically from the canonical fixed stride (fields 0/1: 38-byte items
`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, stride 40) and ABORTS outside
`0 <= index < count` — prover-supplied byte offsets never exist in this
family (soundness guard). §5.3's canonicity is what makes item byte
equality *be* out-ref equality: every out-ref has exactly one encoding.

On any arm's success, `common.finalize`: thread NFT burns
(`Success { burning_token_asset_name }`), the permanent fraud-proof token
mints to the fraud-proof address with `FraudProofTokenDatum
{ fraud_prover }`. The token has no burn path; removal burns the
state-queue node NFT and slashes the operator while the token survives.

**The onchain selector inventory is the adversarial-suite spec** (§8):
distinct-items equality refusal, `i == j` and reversed-pair refusals at
the `first < second` gate, out-of-range abort at the door, disjoint-sets
overlap refusal, fabricated-preimage refusal at the door's commitment
equality, and foreign-anchor refusal at the tx-id re-derivation.

---

## 2. Registration

### 2.1 Category id

**`0000001a`, assigned by the parent orchestration for this wave** — not
self-allocated by next-free grep (three sibling families are being built
concurrently; next-free discovery would race). Recorded as
`INPUT_SET_UNIQUENESS_TEST_CATEGORY_ID_V1` in
`demo/midgard-fault-proofs/tests/support/emulator/harness.ts` with the
inherited "expected, not promised" caveat: the production id is written
only by the registration wave, which re-verifies against standing
reservations at allocation time.

### 2.2 What registration touches

Identical surface list to the decoding plan: SDK catalogue order
(`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`), the `FraudProofs` /
`FaultProofContracts` records, deployment-manifest identity, CLI verbs in
`bin.ts`, `submit-init.ts`'s category union. **None of it is touched by
this working tree.** The family's id appears in exactly two places: the
harness constant and the family contracts record's doc comment; it flows
through `buildCatalogueDeploymentInfo`'s `extraCategories` only.

### 2.3 Script deployment: reference scripts (owner ruling 2026-08-26)

Both steps deploy as reference scripts and are referenced, never attached
inline — the sizes above (6,487 B + 7,571 B) inform envelope arithmetic
only, not the deployment decision. The emulator publishes them via
`publishInputSetUniquenessReferenceScriptsV1` (`oversized: true` plain
publications) and every step submitter takes `referenceScriptUtxo`,
verifying the published hash against the expected step validator before
building (`requireInputSetUniquenessReferenceScriptV1`); inline
attachment remains the no-UTxO fallback only.

---

## 3. Detection

### 3.1 What exists

The committed block's transactions MPF and, per leaf, the compact
structure (validity code + field hashes) with the §5.1 preimages of
fields 0/1 recoverable from the DA layer. The watcher's replay divergence
surfaces exactly this class: the machine's InputSets rule
(`reject_duplicate_input`) refuses the transaction while the operator's
committed leaf says `validity_code == 0`.

### 3.2 Recognition and classification

`scanInputSetUniquenessV1({ spendInputItemCbors, referenceInputItemCbors })`
(`demo/midgard-fault-proofs/src/input-set-uniqueness/scan-v1.ts`)
operates on the decoded §5.3 item lists — the same inputs the field
planner needs — validating each item against the canonical 38-byte shape
(`^825820[0-9a-f]{64}19[0-9a-f]{4}$`) and returning every claim in
canonical order: duplicate spend inputs, duplicate reference inputs, then
the first overlap (lowest spend index per item, then lowest reference
index). The claim schema:

```ts
type InputSetUniquenessClaimV1 =
  | { kind: "duplicateSpendInputs";     firstIndex: bigint; secondIndex: bigint }
  | { kind: "duplicateReferenceInputs"; firstIndex: bigint; secondIndex: bigint }
  | { kind: "spendReferenceOverlap";    spendIndex: bigint; referenceIndex: bigint };
```

`requireInputSetUniquenessClaimV1` returns the single canonical claim and
**throws on an honest transaction** — a prover must never open a thread
it cannot finish. One committed transaction maps to one canonical claim;
multiple co-present faults (a list can hold a duplicate *and* overlap)
are deliberately not multi-proven — one conviction removes the block.

### 3.3 Acceptance gate

Classification only ever fires on leaves the replay read as
`validity_code == 0`; the step-01 submitter re-checks it fail-closed
(`--tx-inclusion.nativeTx carries validity code …`), and the validator
enforces it independently (§1). Three fences, one semantic.

### 3.4 Finding record and routing

No proving core in v1 (§10 D6): a 2-step family with a total local
conviction twin does not need the decoding family's route planner. The
scan → claim → three submitter calls *is* the route. A watcher adapter,
when the registration wave lands one, wraps exactly that sequence.

---

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk/src/fraud-proof/input-set-uniqueness-v1.ts`)

Schema/type/value trios mirroring the lib wire twins byte-for-byte:
`InputSetUniquenessStep01Datum` (= `faultProofStepDatumSchema(Data.Any())`),
`InputSetUniquenessStep01SpendRedeemer`
(= `faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema)` — the
carriage enum directly, no intermediate ctor wrapper; contrast decoding's
`BindNormalTransaction`), `InputSetUniquenessStep02State
{ bad_tx_id: H32 }`, `InputSetUniquenessStep02Datum`, the three-ctor
`InputSetUniquenessStep02Args`, and its `StepRedeemer`. Exported from the
fraud-proof barrel; `catalogue.ts` untouched.

### 4.2 Family modules (`demo/midgard-fault-proofs/src/input-set-uniqueness/`)

- `contracts-v1.ts` — explicit parent-owned `InputSetUniquenessContractsV1`
  (two steps, shared computation-thread and fraud-proof pairs, hub-oracle
  / state-queue / certificate policy ids). **No `categoryId` field**: the
  id is a per-call argument, so a stale record cannot leak an id into a
  transaction. Doc comment pins both parameter orders of §1.
- `scan-v1.ts` — §3.2.
- `submit-common-v1.ts` — thread fetch + token check, reference-script
  verification, step-state parse, family error prefix.
- `submit-input-set-uniqueness-init.ts` — Init mint (asset name
  `0000001a ‖ header_hash` in the emulator), catalogue membership proof
  from the explicit category record, first-step datum
  `{ fraud_prover, data: null }`.
- `submit-input-set-uniqueness-step-01.ts` — both carriages; header-hash
  cross-check against the thread token; compact-CBOR/tx-id/root
  re-derivation; the §3.3 validity refusal; pays
  `State { bad_tx_id }` forward.
- `submit-input-set-uniqueness-step-02.ts` —
  `assertInputSetUniquenessClaimConvictsV1` (the exported local twin of
  every validator predicate: range, `first < second`, byte equality) runs
  before anything is paid for; then plans the needed field opening(s) via
  `planFaultProofFieldOpeningV1` (anchor = the thread's `bad_tx_id`),
  builds the arm's args (duplicate arms: `faultProofFieldOpeningV1`;
  overlap: the planner's compact CBOR + `faultProofFieldCarriageV1`
  twice), and finalizes: thread burn, token mint, datum, both minting
  policies, reference-script-or-attach.

### 4.3 The proving core

Deliberately absent in v1 — see §3.4 and §10 D6.

---

## 5. Carriage frontiers and envelope discipline

Fields 0/1 are 40-byte-stride lists; a §5.1 preimage crosses the
4,095-byte tier-1 boundary only past ~102 items per field. The v1 step-02
submitter therefore plans **tier-1 inline carriage only** (it passes no
`referenceInputs` to the planner); a committed transaction with a
>102-item input list would need tier-2/3 certificate carriage, which the
planner supports and the submitter would refuse today (§10 D5 records the
escalation: wire `referenceInputs`/`certificatePolicyId` through, no
onchain change needed — the door already takes any `FieldCarriageV1`).
Step-01's subject-size frontier is the decoding family's: the
redeemer-carried arm up to the practical compact-CBOR bound, the
published-chunk arm beyond it; both arms exist on-chain and offchain
(§10 D4).

---

## 6. Economics and pacing

Three small L1 transactions (§8's proof-fit capture pins all three under
the emulator's execution maxima) plus removal. Min-ada for the thread
UTxOs returns at finalize; the field openings are tier-1 so no
certificate min-ada is parked. No pacing logic: the chain is three
sequential confirmations and the family has no self-loop.

---

## 7. Cancel, recovery, and the corners

### 7.1 Crash-resume

Thread state is fully recoverable from the chain: `stepIndex` + parsed
step datum (`requireInputSetUniquenessStepStateV1`) tell a restarted
prover exactly where the thread stands; every submitter takes
`threadOutRef` and re-verifies rather than trusting local state.
`ct.Cancel` on both steps refunds an abandoned thread to the prover.

### 7.2 Corner cases (all pinned by tests)

- **(a) Adjacent duplicates and first/last positions** — the predicate is
  position-blind; Aiken pins adjacent `[o5,o5]` and non-adjacent
  `[o7,o3,o7]` → `(0,2)` (first/last of a 3-list).
- **(b) `i == j` refused** — a reflexive pair is trivially "equal";
  `expect first_index < second_index` refuses it (and reversed pairs)
  before any comparison. Pinned in Aiken and on-emulator.
- **(c) Same index in two different lists is NOT the fault** — overlap
  carries no index relation; only out-ref byte equality convicts. Pinned
  positively (`spend[1] == ref[1]` convicts because the bytes match) and
  negatively (disjoint lists refuse at the same indices).
- **(d) Honestly-rejected leaf never binds** — `validity_code != 0`
  refused locally and by the validator (§1, §8 adversarial suite).
- **(e) Out-of-range index** — aborts at the door's arithmetic item
  addressing, never clamps.
- **(f) Fabricated preimage / foreign anchor** — refused by the door's
  commitment equality and the compact-CBOR tx-id re-derivation (Aiken
  step-02 refusals).

### 7.3 What is NOT this family

Empty spend-input sets (zero-input family), cross-block double spends
(double-spend family), and the wrongful-rejection direction (an operator
who *rejected* a unique-input transaction — `l2-tx-mistag` /
`validationTraceDispute` territory).

---

## 8. Testing

### 8.1 Where and how

Aiken: 17 tests in the two validator files — 6 step-01 (Continue happy
path, published-chunk 22-level binding, rejected-leaf refusal,
forged-root refusal, Cancel pair) and 11 step-02 (4 convictions: the §7.2
positive corners across all three arms; 7 refusals: the §1 selector
inventory). `aiken check -m 'input_set_uniqueness/'` — note the trailing
slash; the bare module filter is vacuous.

Emulator (vitest, per-file isolation for the uplc wasm heap):

- `tests/submit-init-emulator-input-set-uniqueness-lifecycle.test.ts` —
  real-fault polarity. Spend/spend end-to-end: init → step-01
  (redeemer-carried, reference script) → scan/claim → step-02 → token
  datum/out-ref assertions → **removal leg** (explicit category,
  `requireReferenceScripts`, state-queue NFT burned, root `next ==
  Empty`, operator slashed, scheduler `NoActiveOperators`, token retained
  at the same out-ref, second removal refused) — plus proof-fit capture
  on all three stages. Ref/ref and spend/ref journeys through the
  decisive step-02 conviction, each on its own harness (one thread per
  committed header: the thread asset name is `categoryId ‖ header_hash`).
- `tests/submit-init-emulator-input-set-uniqueness-adversarial.test.ts` —
  adversarial polarity against an honest all-unique commitment: scan
  finds nothing; init + step-01 land (binding is not conviction); the
  honest submitter refuses every fabricated claim locally; and raw
  finalize builders (test-support only — production never takes these
  paths) drive fabricated-duplicate, `i == j`, out-of-range, and
  disjoint-overlap claims to the validator, each refused at the exact
  check (`expectOnchainRefusalV1`'s `/failed script execution/` honesty
  guard). Second front: the TxIsInvalid leaf of §7.2(d), refused locally
  and by a raw bind on-chain, thread pinned still parked at step-01.

### 8.2 Test-support extensions (extend, do not fork)

`tests/support/emulator/contracts.ts` gains
`buildInputSetUniquenessChainV1` + the `realInputSetUniqueness` flag
(assembly mirrors the decoding block: shared thread/fraud-proof pairs,
address round-trip through `Data`); `harness.ts` the id constant and the
`extraCategories` spread; `removal-deployment.ts` the
`fraudProofInputSetUniqueness` manifest entry. The family's own fixture
module `tests/support/input-set-uniqueness-emulator-v1.ts` builds
committed transactions with caller-chosen fields 0/1 by **direct
materialization** (`materializeMidgardNativeTxFromCanonicalV1` — the
generic `makeNativeTx` helper cannot express canonical reference-input
lists), commits them beside a decoy leaf in a real MPF trie, and carries
the harness/scenario/publication/removal-category helpers plus the two
raw builders.

### 8.3 What lands at registration

CLI verbs, catalogue append, `submit-init.ts` union arm, deployment
manifest identity, watcher adapter — none in this tree.

---

## 9. Sequencing and dependencies

Built in this order, each gate green before the next: (1) onchain family
+ Aiken tests + blueprint regeneration (patched fork, `aiken build --env
testnet`); (2) SDK schemas + dist rebuild; (3) family src modules; (4)
test-support wiring; (5) emulator suites both polarities; (6) this
document + the catalogue-status rows. No dependency on the sibling
value-not-preserved / mint-authorization waves beyond the disjoint id
assignments.

---

## 10. Decision register

Decided under the AGENTS.md north star (correctness, safety, liveness,
performance, convenience); each entry records what would reopen it.

- **D1 — Two steps.** Zero-input proves in 2 (compact structure alone);
  input-no-idx needs 4 (anchor → select → adjudicate against L1 state).
  This predicate needs field openings but no external state and no lift,
  so conviction and finalize coincide: bind (01) + open-and-convict (02).
  A third step would carry state and decide nothing. Reopens only if a
  future carriage tier forces splitting the overlap arm's double opening
  out of the finalize envelope — no current measurement suggests it.
- **D2 — Deployment shape: reference scripts (OWNER RULING 2026-08-26).**
  Not delegated; not reopenable by measurement.
- **D3 — Own init submitter** rather than widening the generic
  `submitFabricatedFamilyInitV1`: the family record is explicit-contract
  shaped (§2.2) and the decoding family set the per-family-init
  precedent; a shared widened helper would put a category-id parameter
  back into a generic surface that pre-registration discipline keeps
  narrow. Reopens at the registration wave, which may fold inits.
- **D4 — Step-01 supports both carriages offchain**, mirroring the
  onchain selector — unlike missing-signature's bare-args-only step-01,
  the arm exists on-chain here, and shipping only one leg would strand
  it untested. The published-chunk leg is pinned in Aiken; the emulator
  drives the redeemer-carried leg (subject sizes in the suites never
  need chunking).
- **D5 — Tier-1-only openings in the v1 step-02 submitter.** Fields 0/1
  cross tier-1 only past ~102 items per field (§5); the submitter plans
  inline carriage and fails closed beyond it. Reopens on the first
  observed >102-item committed input list: wire the planner's existing
  tier-2/3 support through — offchain-only change.
- **D6 — No proving core / route planner in v1** (§3.4): scan → claim →
  three calls is the whole route; a core would be indirection without a
  decision to encapsulate. Reopens when a watcher adapter lands and
  wants crash-resume orchestration above the per-step submitters.
- **D7 — Category id `0000001a` is parent-assigned** for this concurrent
  wave (§2.1), superseding next-free grep discovery for this family
  only; the "expected, not promised" caveat stands unchanged.
- **D8 — Fixture realism by direct materialization** (§8.2): the fixture
  encodes real canonical items through the core codecs and real MPF
  proofs; no synthetic leaf shapes. The 38-byte §5.3 item shape is
  asserted at scan time, so a drifting codec fails loudly offchain
  before it fails cryptically on-chain.
- **D9 — Raw builders live in test support only.** Production submitters
  keep every fail-closed guard; the guard-free transaction builders the
  adversarial suite needs are deliberately unreachable from
  `src/` exports.

---

## 11. Out of scope

- Any onchain change beyond the new family: no edits to
  `validation-machine-v1.ak` or any existing validator.
- The registration wave's execution (§2.2, §8.3) — no CLI verbs, no SDK
  catalogue append, no `submit-init.ts` union change, no deployment
  manifest identity until then.
- The zero-input, double-spend, value-not-preserved, and
  mint-authorization families (the latter two are concurrent sibling
  waves with their own plans and ids).
- The wrongful-rejection direction (§7.3).
- Slashing economics and enabling autonomous proving in any deployed
  watcher.
- GOAL_PROGRESS ledger rows (owner may want one for this landing).
