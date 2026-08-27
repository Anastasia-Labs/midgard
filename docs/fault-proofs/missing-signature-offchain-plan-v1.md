# Missing-signature fault: offchain implementation plan (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `b1223571`) plus
the uncommitted 2026-08-26 working tree (the native-script-decoding
removal-leg extension this plan builds on — see §9 gate 1). Task: **Q16**
(`GOAL_SPEC.md` §9.3; depends Q00–Q03, C43 — all satisfied at HEAD).
Catalogue row: `catalogue-status.md` §1 row 8. This document began as the
implementation plan and now also records the completed pre-registration wave.
It still registers and deploys nothing in production.

**Completion record (2026-08-26).** The planned SDK, detector, proving core,
reference-script submitters, isolated `0000000e` harness, positive lifecycle
through permanent proof mint and fraudulent-block removal, honest-commitment
on-chain refusal, and negative/cancel/resume suites are implemented. The
initial envelope run exposed step-04's whole-field memory gap, so this wave
also implemented the plan's required escalation: a 32-witness authenticated
absence-walk batch, canonical thread-committed checkpoint, deterministic
Scan/Finalize handoff, and interior cancel/resume. The emulator now proves the
first automatic tier-2 case (140 witnesses) and the maximum admissible field-7
vector (318 witnesses), while the worst-depth step-01 transaction and every
submitted scan/finalize transaction stay below 13.2M memory and 8B CPU.
Production catalogue/manifest/CLI/watcher-family registration remains the
separate registration wave exactly as planned. Where the original prospective
text below discusses the whole-field step-04 design or an open escalation,
this completion record and the updated contract sections supersede it.

The parity bar is the `native-script-decoding` family (row 18) as planned in
`native-script-decoding-offchain-plan-v1.md` and built on this branch: a
consumer-agnostic proving core with CLI and watcher adapters, per-arm step
submitters plus cancel, a pre-registration emulator harness under a reserved
test category id, and lucid-evolution emulator suites in both polarities —
through fraud-proof mint **and** fraudulent-commitment removal — plus a
compiled-size/frontier envelope suite. Everything that family's plan decided
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
- **Pre-registration explicit-record discipline:** pre-registration
  families must not route their ids through the deployment manifest —
  `parseFraudProofCatalogueDeploymentInfo` silently drops non-canonical
  keys (`catalogue-status.md` §3). Contracts records are explicit and
  parent-owned; the SDK catalogue, `submit-init.ts`'s category union, and
  `bin.ts` are untouched until the registration wave.
- **Reserved ids are expected, not promised** (decoding plan §10 Q2): the
  test-harness constant records the expected next-free index; the
  production id is written only by the registration wave, which re-verifies
  "next free after standing reservations" at allocation time.
- **Removal via explicit category** (2026-08-26 working tree):
  `remove-fraudulent-block.ts`'s `RemoveFraudulentBlockExplicitCategory` /
  `buildExplicitCategoryRemovalContracts` / `assembleRemovalContracts` seam
  lets a pre-registration family drive removal with every fail-closed check
  intact and **zero** further changes to that module. The fraud-proof token
  is permanent by design (the state-queue node NFT burns; the token
  survives as evidence and as the `alreadyProven` gate).
- **Cancellation is an explicit prover decision** (decoding
  `submit-native-script-decoding-cancel.ts` policy): the proving core never
  cancels on its own; an unexpected abort surfaces as a `stalled` outcome.

All `file:line` anchors are against HEAD `b1223571` (+ working tree where
noted).

---

## 1. The contract the builders must satisfy

The as-built onchain family
(`onchain/aiken/validators/fraud-proofs/missing-signature/step-0{1..4}.ak`,
lib wire twins `lib/midgard/fraud-proofs/missing-signature/step-0{1..4}.ak`)
is the byte-for-byte target. The fault statement: a transaction the operator
committed as accepted names a required signer (body field 4) whose witness
is absent from the address-witness collection (witness field 7)
(`step-04.ak:109-124`). This is a **single-direction** family — wrongful
acceptance only; see §3 for where the other direction lands.

**Step chain** `Init → 01 → 02 → 03 → 04.Scan* → 04.Finalize`, `ct.Cancel`
on every position, including an interior step-04 checkpoint. Step-04 is the
only self-loop. Parameterization (acyclic, applied backwards, step-04 first):

| Validator                                | Parameters (blueprint-declared order)                                                                                                    |
| ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `fraud_proofs/missing_signature/step_01` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`                                                      |
| `…/step_02`                              | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id`                            |
| `…/step_03`                              | `step_04_validator_script_hash`, `computation_thread_token_policy_id`                                                                    |
| `…/step_04`                              | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id` |

Note against the decoding family's table: **two** steps here take the
field-preimage certificate policy (02 and 04 — each opens a different field
through the §8.8 door), and step-04 leads with the fraud-proof pair. The
contracts record's docblock pins this order and the envelope suite asserts
it via distinct applied hashes (#609 checks arity only, not order).

**Step-01** (`step-01.ak`): `Continue(NativeTxInclusionArgs)` — the shared
native binding (`pass_native_tx_to_next_step`, counted `transactions_root`
authentication + PHAS membership over the raw compact CBOR). **Bare args
only** — this step has no published-chunk carriage arm on-chain (see §5).
Output at step-02's address:
`step_02.State { verified_tx_id, verified_witness_set_hash }` — and the
second field is the family's load-bearing move: §3's transaction id commits
the _body_ alone, so step-01 reads the real `witness_set_hash` off the
compact structure the block committed, and it rides the thread as the §2.5
anchor from here on (`step-01.ak:56-70`). No redeemer downstream can
restate it.

**Step-02** (`step-02.ak`): opens body field 4 (`required_signers`) through
the §8.8 door (`opened_field_view`, `BodyAnchor { tx_id: verified_tx_id }`)
and selects the accused signer hash by `bad_required_signer_hash_index` via
the fixed 28-byte stride (`field_item_at` — out-of-domain ordinal ABORTS,
never clamps). Args: `{ input_index, output_index,
required_signers_opening: FieldOpeningV1, bad_required_signer_hash_index }`.
Output: `step_03.State { missing_required_signer_hash, verified_tx_id,
verified_witness_set_hash }`.

**Step-03** (`step-03.ak`): the vkey lift — the prover supplies
`missing_required_signer_vkey` and the step checks
`get_verification_key_hash(vkey) == missing_required_signer_hash`
(blake2b-224, `common/utils.ak:783`). Cheapest step; no
field opening, no reference inputs. Output: `step_04.State
{ missing_required_signer_vkey, verified_tx_id, verified_witness_set_hash,
field_walk_checkpoint_hash: #"" }`.
The need for a vkey _preimage_ here creates the family's one completeness
corner (§7.2).

**Step-04** (`step-04.ak`): opens witness field 7
(`address_witnesses`) through the door with
`WitnessAnchor { tx_id: verified_tx_id, witness_set_hash:
verified_witness_set_hash }` — the door re-derives the supplied witness set
against the **thread-anchored** hash, which is the entire soundness of the
step (`step-04.ak:84-97`): a door checking only the tx id would accept an
empty witness set against any transaction, making "the signature is absent"
true of everything ever committed. `Scan` consumes exactly 32 witnesses,
refuses the accused vkey, and self-loops with the hash of a canonical 53-byte
checkpoint binding tx id, field 7, total length, item count, next cursor, and
next byte offset. A resumed spend supplies those exact bytes; the shared field
opening core re-authenticates the field and reconstructs the only valid cursor
and offset before continuing. `Finalize` is legal only when at most 32 items
remain; it proves absence over the suffix, calls `common.finalize`, burns the
thread NFT, and mints the permanent fraud-proof token. The two args arms are
`Scan { input_index, output_index, addr_tx_wits_opening, checkpoint_cbor }`
and `Finalize { input_index, output_index,
fraud_proof_mint_redeemer_index, addr_tx_wits_opening, checkpoint_cbor }`.
Initial state accepts only the empty checkpoint commitment with `None`; resume
accepts only the committed canonical bytes, so malformed, stale, foreign, or
unreachable handoffs fail closed.

**The onchain selector inventory is the adversarial-suite spec.** The
validator files carry the exact forgeries the emulator suites must
reproduce at the transaction level (§8.2 suite 4): the substituted signer
list and out-of-range index (`step-02.ak` selectors), the one-sided forged
witness set, the **both-sides slashing forgery** (genuine body bytes ‖
empty-witness-set tail — every id-rooted check passes; only the
thread-anchored `verified_witness_set_hash` refuses it,
`step-04.ak:301-332`), its substituted-witness-set variant, and the
**tier-3 decoy carriage** (256 decoy witnesses under a foreign certificate,
refused by the door's mint-welded `field_hash` equality against the
anchored commitment — the #606/E2 repair, `step-04.ak:365-415`).

---

## 2. Registration

### 2.1 Category id

Standing reservations at HEAD: `0000000b` fabricated-deposit
(`fabricated-deposit-v1.ts:59`), `0000000c` fabricated-withdrawal
(`fabricated-withdrawal-v1.ts:85`), `0000000d` native-script-decoding
(test id, `tests/support/emulator/harness.ts:313`). **This family's
expected index is `0000000e`** (index 14; repo-wide grep confirms the id is
unclaimed). Per the inherited ruling the constant lands as
`MISSING_SIGNATURE_TEST_CATEGORY_ID_V1` in the test harness with the
"expected but not promised" caveat; the production id is written only by
the registration wave.

### 2.2 What registration touches

Identical surface list to the decoding plan §2.2 (SDK catalogue order,
`FraudProofs`/`FaultProofContracts` records, deployment-manifest identity,
node manifest/descriptors/catalogue-MPF build, CLI category parse,
inspect-contracts unions, watcher `families[]`, test re-pins per §8.3).
Registration is a fresh genesis-level deployment (catalogue immutability,
D-S13). Nothing in this plan's builder wave moves any pinned surface.

### 2.3 Script deployment: reference scripts (owner ruling 2026-08-26)

**Owner ruling 2026-08-26: fault-proof step validators always deploy as
reference scripts and are referenced, never attached inline.** All four
steps deploy as reference scripts, joining the family-steps
`referenceScriptTargets` class the decoding family's plan established
(its §2.3 Q3); the emulator harness publishes them via
`publishPlainReferenceScriptUtxo` exactly as
`publishDecodingReferenceScriptsV1` does, and every submitter sources its
spending validator by reference with the fail-closed
`require…ReferenceScriptV1` hash check (the decoding
`submit-common-v1.ts:105` shape). The per-step envelope assertions in
`inspect-contracts.test.ts` take the reference-script-deployment variant
for this family, as they do for decoding.

Compiled sizes are pinned by the envelope suite against a normal blueprint
build with the repository's pinned Aiken fork. They no longer decide the
deployment shape; they feed the frontier chart (§5) and the registration
wave's reference-script publication accounting:

| Step    | Unapplied compiled size |
| ------- | ----------------------- |
| step-01 | 5,775 B                 |
| step-02 | 6,695 B                 |
| step-03 | 1,510 B                 |
| step-04 | 9,327 B                 |

A welcome consequence: with no inline validator riding the spend, the
whole 16,384-byte envelope (minus fixed transaction overhead) is
available to each step's redeemer, which widens all three §5 frontiers —
the real worst-depth step-01 transaction gate confirms that its subject
frontier fits.

---

## 3. Detection

### 3.1 What exists

The divergence this family proves is **already computed** on every block
replay: the TS validation twin emits
`RejectCodes.MissingRequiredWitness = "E_MISSING_REQUIRED_WITNESS"`
(`demo/midgard-validation/src/types.ts:36`, raised at
`src/phase-a.ts:331`), and the watcher's `block-replay.ts` /
`phase-a-verifier.ts` recompute `canonicalOperatorValidity` against the
committed validity per event. Unlike decoding's direction-A sweep, no new
standing scan work is added — detection here is a _classification_ of a
divergence the replay already surfaces (§10 D5).

### 3.2 Recognition and classification

A finding candidate is a committed **accepted** transaction (Normal leaf
with embedded scalar 0, or a `ForcedTxValid` forced leaf) whose replay
raises `MissingRequiredWitness`. The detector then classifies:

1. Decode the committed compact structure; enumerate field 4's signer
   hashes; recompute per-witness `blake2b_224(verification_key)` over
   field 7.
2. For each required hash with no matching witness:
   - a vkey preimage for the hash is **known** (§3.3) →
     `MissingWitness` — provable; the accused index is that hash's ordinal
     in field 4 (any one absent signer suffices; choose the first).
   - no preimage recoverable → `UnknownVkeyPreimage` — **refused at the
     API boundary** (§7.2); journaled, routed to the interactive residual.
3. A required hash whose witness is _present_ but whose signature fails
   verification is **not this family's fault** — classify
   `PresentButInvalid` and route to the `invalidSignature` family (Q15),
   which proves exactly that (§10 D6).
4. Replay divergence absent (honest commitment) → `NotAFault` — refused.

The other direction — an operator _rejecting_ a transaction by falsely
claiming a missing signature — is out of this family's scope by
construction (the family has no verdict-adjudication arm); it lands where
`catalogue-status.md` §6 already places wrongful rejection: the
`l2-tx-mistag` censorship gap and the `validationTraceDispute` machine
replay. Recorded, not planned here.

### 3.3 Vkey recovery

Step-03 needs the 32-byte vkey whose blake2b-224 is the accused hash. The
recovery helper tries, in order: (a) witness sets of other committed L2
transactions (any block, either polarity — the indexer's material); (b) L1
witness sets the watcher has observed; (c) an operator-supplied preimage
(the injured required signer knows their own key; a manual CLI invocation
can carry it). The helper is a detection-side capability; the finding
record carries the resolved vkey so the proving core never searches.

### 3.4 Finding record and routing

`MissingSignatureFindingV1` — the typed contract between detection and
proving, mirroring `NativeScriptDecodingFindingV1` (`finding-v1.ts`):
header hash, event key, the committed compact CBOR reference, the accused
signer ordinal and hash, the resolved vkey, the witness-set hash read off
the committed structure, and the provability class
(`MissingWitness` | `PresentButInvalid` | `UnknownVkeyPreimage` |
`NotAFault`; provable set = `{MissingWitness}`).
`assertMissingSignatureFindingProvableV1` is the boundary gate the core
calls — classification refusals are non-negotiable, no policy overrides
them. Manual and autonomous consumers take the same record (§4.3). The
watcher `families[]` entry lands at registration.

---

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk/src/fraud-proof/missing-signature-v1.ts`)

One new module, mirroring `invalid-signature.ts` +
`native-script-decoding-v1.ts`:

- `missingSignatureThreadTokenAssetNameV1(categoryId, headerHash)` — id
  parameterized until registration; no category-id constant pre-wave.
- Per-step `State`/`Datum`/`Args`/`SpendRedeemer` schemas from the shared
  generics (`faultProofStepDatumSchema`/`faultProofStepRedeemerSchema`,
  `native.ts:228-248`), field-for-field against the lib twins:
  - step-01 `Args = NativeTxInclusionArgsSchema` (`native.ts:32-50`,
    exists);
  - step-02 `State { verified_tx_id, verified_witness_set_hash }`,
    with args for the field opening and bad-signer ordinal;
  - step-03 `State` (3 fields), `Args { input_index, output_index,
missing_required_signer_vkey }`;
  - step-04 `State` adds `field_walk_checkpoint_hash`; `Args` is an enum of
    `Scan` and `Finalize`, both carrying the field opening and checkpoint;
    only `Finalize` carries the fraud-proof mint redeemer index.
- The checkpoint helpers encode the canonical 53-byte field-walk position,
  hash it under the Aiken domain separator, and resolve only the deterministic
  32-item schedule. `FieldOpeningV1Schema` and the inclusion-args schema remain
  shared with the invalid-signature and input-no-idx chains. The field indices are already
  pinned offchain (`MIDGARD_FIELD_INDEX_V1.requiredSigners = 4`,
  `.addressWitnesses = 7`, `field-opening-v1.ts:78,81`) with the
  Aiken-drift gate in `field-opening-v1.test.ts:34`.
- A `missingSignatureVkeyHashV1` helper (blake2b-224 twin of
  `get_verification_key_hash`) if `@al-ft/midgard-core` does not already
  export one — check `blake2b-224-trace-v1.ts` first (prefer reuse).
- `contracts.ts` chain-builder additions land **at registration**.

### 4.2 Family modules (`demo/midgard-fault-proofs/src/missing-signature/`)

Mirroring the decoding module set, with the smaller fixed-stride step-04 loop:

| Module                                    | Responsibility                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | Template                                                                                                                                                                                                                                                         |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `contracts-v1.ts`                         | blueprint titles (`fraud_proofs/missing_signature/step_0N.main.spend`), `MissingSignatureContractsV1` record (steps 4-tuple, computationThread, fraudProof, hubOraclePolicyId, stateQueuePolicyId, fieldPreimageCertificatePolicyId), parameter-order docblock, **no categoryId field**                                                                                                                                                                                                                                                                                                                                                                                                                                            | decoding `contracts-v1.ts`, near-verbatim                                                                                                                                                                                                                        |
| `finding-v1.ts`                           | §3.4 record, provability enum, `assert…ProvableV1` boundary gate                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   | decoding `finding-v1.ts` structure                                                                                                                                                                                                                               |
| `evidence-v1.ts`                          | step-01 inclusion evidence (reuse `parseSubmitStep01TxInclusion`, `submit-step-01.ts:236`, and the PHAS helpers `runtime.ts:1088/1205`); the field-4 opening plan (**first offchain consumer of field 4** — `planFaultProofFieldOpeningV1` with `fieldIndex: requiredSigners`, BodyAnchor); the field-7 opening plan (WitnessAnchor — the `submit-invalid-signature-step-02.ts:395-407` shape: `witnessSet: witnessSetCompact`, `anchorWitnessSetHash` **taken from the thread state, never re-derived locally**); accused-ordinal selection; carriage publication via `publishFaultProofFieldCarriageV1` + `faultProofFieldCarriageReferenceOrderV1` when the tier demands it (§5). Owns nothing the door builders already own    | decoding `evidence-v1.ts` delegation discipline                                                                                                                                                                                                                  |
| `submit-common-v1.ts`                     | label-prefixed errors, `require…ThreadUtxoV1` (thread NFT via the shared `requireComputationThreadToken`), `requireMissingSignatureReferenceScriptV1` (the published reference-script UTxO must hash to the step being spent — §2.3), fail-closed `require…StepStateV1` datum reads                                                                                                                                                                                                                                                                                                                                                                                                                                                | decoding `submit-common-v1.ts`, near-verbatim                                                                                                                                                                                                                    |
| `submit-missing-signature-init.ts`        | fork of `submit-init.ts`'s generic tail taking the explicit contracts record + catalogue category (id ‖ headerHash asset name, first-step datum `{fraud_prover, data: null}`, Init mint with membership proof, PHAS zero-withdrawal). Collapses back into `submit-init.ts` at registration                                                                                                                                                                                                                                                                                                                                                                                                                                         | decoding `submit-…-init.ts`                                                                                                                                                                                                                                      |
| `submit-missing-signature-step-01..04.ts` | one submitter per step. Steps 01–03 have one Continue arm; step-04 deterministically chooses Scan or Finalize from authenticated field size and the thread's checkpoint. Every locally predictable validator abort is refused **before anything is paid for**, message naming the check — step-02 pre-checks the ordinal against the decoded field-4 count and the preimage against the committed commitment; step-03 pre-checks `blake2b_224(vkey)` against the thread's hash; step-04 validates the entire absence predicate locally, resolves only a reachable checkpoint, and publishes/reads the required carriage. Positional indices are resolved against the built transaction (`requireInputIndex` etc.), never hardcoded | step-01: `submit-invalid-signature-step-01.ts:300-390`; step-02: `submit-input-no-idx-step-02.ts` + the field-4 index; step-03: `submit-fabricated-deposit-step-03.ts` handoff shape; step-04: shared field opening + bounded self-loop + terminal finalize/mint |
| `submit-missing-signature-cancel.ts`      | step-agnostic `ct.Cancel` (locates the step by address, burns via `BurnForCancellation`, refuses non-prover signers up front)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | decoding cancel, directly copyable                                                                                                                                                                                                                               |
| `prover-v1.ts`                            | the consumer-agnostic core (§4.3)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | decoding `prover-v1.ts` quartet                                                                                                                                                                                                                                  |
| `prover-adapters-v1.ts`                   | CLI one-shot (permissive policy — "the operator IS the policy") + watcher fiber (config/summary; mounting at registration)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | decoding adapters, directly copyable                                                                                                                                                                                                                             |
| `index.ts`                                | barrel; one `export *` line added to `src/index.ts`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | —                                                                                                                                                                                                                                                                |

### 4.3 The proving core

```ts
proveMissingSignatureFaultV1(
  finding: MissingSignatureFindingV1,   // §3.4 — the sole input
  deps: MissingSignatureProverDepsV1,
): Effect<MissingSignatureProofOutcomeV1>
```

The decoding core's four-point contract, inherited whole: capability-
injected (zero consumer coupling); resumable and
idempotent-by-reconstruction; policy as data
(`minSettlementDepth` / `maxThreadBudgetLovelace` / single-flight / dedup —
same defaults discipline; the maturity guard is trivial here); outcome as
data (`proven {fraudProofUnit, fraudProofOutRef,
txHashes}` | `refused {classification|policy|duplicate|alreadyProven}` |
`stalled`). Resume is strictly simpler than decoding's: locate the thread
UTxO by asset name across the four step addresses; the holding address
identifies steps 01–03, while step-04's committed checkpoint identifies its
only reachable deterministic cursor. Re-derive the evidence from the finding,
resolve that cursor, and continue. Only the
§3.2 provability classification is non-negotiable at the API boundary.

---

## 5. Carriage frontiers and envelope discipline

The genuinely family-specific offchain surfaces are carriage arithmetic and
the fixed-stride absence walk. Three redeemer frontiers are pinned by §8.2
suite 1:

- **Step-01 — the subject's compact CBOR rides the redeemer.**
  `NativeTxInclusionArgs` carries `native_tx_compact_cbor` whole, and the
  step has **no published-chunk arm on-chain** (unlike decoding step-01).
  The maximum-depth transaction gate submits the real reference-script spend,
  not a size-only proxy, and proves it stays below the transaction and ExUnits
  limits. No step-01 chunked-carriage escalation was needed.
- **Step-02 — field 4 is small by construction.** §5.3 fixes the item at a
  raw 28-byte stride, so the tier-1 inline preimage is `28 × n` bytes;
  even adversarial signer counts stay tier-1 far past realistic bounds.
  Chart it anyway (the count bound is tx-size-derived,
  `consensus-profile-v1.ts:204-209`).
- **Step-04 — field 7 is the fat one.** Address witnesses are ~103 B each
  and the collection is bounded only by the committed transaction's own
  byte budget, so a worst-case preimage rivals the whole L1 envelope. The
  tier is **never a caller decision** — `planMidgardFieldCarriageV1`
  chooses from preimage length; the builder passes through
  `faultProofFieldCarriageV1` and, at tier 2/3, publishes chunks via
  `publishFaultProofFieldCarriageV1` (content-addressed reuse) with §8.4
  reference ordering. Tier-3 security is the door's welded-hash equality
  (§1); the emulator adversarial suite reproduces the decoy-carriage attack
  against it. The validator then consumes at most 32 fixed-stride items per
  transaction. The 140-witness first automatic tier-2 case takes four scans
  plus one finalization; the maximum admissible 318-witness vector takes nine
  scans plus one finalization.

**ExUnits:** suite 1 captures every submitted transaction and enforces the
13.2M-memory/8B-CPU basis as well as the transaction-byte limit. The original
whole-field fold exceeded the first automatic tier-2 memory frontier, which
triggered the required bounded-walk redesign. With a 32-item batch, the
140-witness and maximum 318-witness journeys pass. The separately required
worst-depth step-01 gate also submits and passes the real transaction.

---

## 6. Economics and pacing

Thread shape is Init + steps 01–03 + `ceil(max(1, witness_count) / 32)`
step-04 transactions: 5 L1 transactions for a one-batch subject, 9 for the
140-witness frontier, and 14 for the maximum admissible 318-witness vector
(plus content-addressed tier-2/3 carriage publications where needed). This is
small against the 75,000-ADA `fraud_prover_reward` profile. Funding and
idempotency rules inherit from decoding plan §6: the prover wallet funds
fees/min-ADA/collateral; every submitter reconstructs from chain state; on
rollback the core re-queries the thread by asset name and re-resolves the
committed checkpoint.

---

## 7. Cancel, recovery, and the corners

### 7.1 Crash-resume

Locate by asset name `category_id ‖ header_hash` across the four step
addresses. The address identifies steps 01–03; at step-04 the datum's
checkpoint hash resolves to the only cursor reachable by the 32-item schedule.
The exact checkpoint bytes are re-derived from authenticated evidence and
supplied on resume. Cancel at every step, including an interior step-04
position, reclaims min-ADA and burns the NFT (prover-signed).

### 7.2 The unknown-preimage corner (recorded; refused at classification)

Step-03 demands the vkey **preimage** of the accused 28-byte hash. An
operator who commits an accepted transaction requiring a garbage hash (no
known preimage) has committed exactly this family's fault, but no
single-party prover can walk step-03. Disposition:

- The detector classifies `UnknownVkeyPreimage` after §3.3 recovery fails;
  the finding is journaled and **refused at the proving API boundary** —
  never silently dropped.
- The rule remains enforced and adjudicable: the machine's Signatures
  phase (`reject_missing_required_witness`,
  `validation-machine-v1.ak`) compares hash against hash-of-witness-vkey
  and needs no preimage, so the interactive `validationTraceDispute`
  covers the corner today.
- The single-party close would be an onchain amendment — a step-03/04
  variant folding `blake2b_224` over each witness vkey and comparing
  hashes, needing no preimage. Deliberately **not** proposed in this
  plan (it is a family redesign, priced in per-item hashing); recorded so
  the option is on the table if garbage-hash commitments are ever observed
  in the wild (§10 D4). Under GOAL_SPEC §3 invariant 3 this is the same
  shape as the W-C14 residuals already tracked in `catalogue-status.md`
  §6.

### 7.3 Present-but-invalid routing (recorded, not open)

A required signer whose witness exists but fails ed25519 verification is
`invalidSignature`'s fault (Q15), not this family's — step-04's fold would
find the key present and refuse. The detector's classification table is
total across the two signature families; a thread accidentally started on
a present-witness transaction is refused by the step-04 submitter's local
fold before anything is paid for, and on-chain at
`required_signature_is_present == False` if forced raw.

---

## 8. Testing

**Standing requirement (owner directive 2026-08-25)** restated: emulator
suites in both polarities — the real fault proves through mint **and**
removal; the adversarial prover against an honest commitment is refused
on-chain at the exact check. Suite 3 carries the positive polarity;
suite 4 the adversarial one.

### 8.1 Where and how

- Own test files per facet (`@lucid-evolution/uplc` wasm heap; vitest
  isolates per file — every decoding suite's closing docblock line applies
  verbatim). Names: `tests/submit-init-emulator-missing-signature-<facet>.test.ts`.
- Pre-registration harness: the `extraCategories` seam
  (`tests/support/emulator/catalogue.ts:61-124`) — extras are a sidecar,
  never merged into `categories`; base roots/proofs stay byte-identical, so
  **no measured fixture moves**.
- Blueprint: read-only from `onchain/aiken/plutus.json` (or
  `MIDGARD_REAL_BLUEPRINT_PATH`); never rebuilt by tests. Suites probe for
  `fraud_proofs/missing_signature/step_01.main.spend` at collection time
  and skip with a named reason if the local blueprint predates the family
  (the `route-freedom-journey.ts:104-115` idiom). All parameter
  application through `applyCompiledScript` (arity-checked, memoized);
  **never name `compiledCode`** outside the envelope suite — the #610 gate
  (`zz605-semantic-resolver-arity.test.ts:358`) is a text scan, and the
  envelope suite's measurement-only read needs its own allowlist entry
  (the `6ab8b198` precedent).

### 8.2 Suites, in order of construction

1. **Envelope/frontier + ExUnits measurement**
   (`tests/missing-signature-envelope-v1.test.ts`): pins
   the four compiled sizes (§2.3 table is its first datum), asserts the
   parameter-order distinct-hash checks, charts the three redeemer
   frontiers of §5 under the reference-script deployment shape (§2.3),
   submits the 140-witness automatic tier-2 and maximum admissible
   318-witness journeys through bounded scans, and submits the worst-depth
   step-01 transaction. Every submitted transaction is checked against the
   byte and ExUnits limits. The suite exposed and then closed the step-04
   whole-field completeness escalation; step-01 required no redesign.
2. **SDK schema twins**: each `State`/`Args` schema round-trips against
   CBOR vectors derived from the lib twins; the field-index drift gate
   already exists.
3. **Lifecycle, positive polarity**
   (`…-missing-signature-lifecycle.test.ts`): commit a block whose
   accepted transaction requires a signer with no witness; drive the
   **proving core** off a §3.4 finding through Init → 01 → 02 → 03 → 04;
   assert the fraud-proof token mints with the thread's asset name and the
   NFT burns; re-invocation returns `refused: alreadyProven`. Then the
   **removal leg** via the explicit category
   (`RemoveFraudulentBlockExplicitCategory` +
   `MISSING_SIGNATURE_REMOVAL_DEPLOYMENT_ENTRY_V1`): state-queue node NFT
   burned, root `next === "Empty"`, operator slashed
   (`SlashActiveOperator`), scheduler cleared, **fraud-proof token
   retained at the same out-ref**, second removal claim finds nothing. A
   second test drives the same journey through the **per-step submitters
   directly** — the two planes pinned against each other (decoding
   direction-a precedent). The envelope suite covers the tier-2/3 field-7
   carriage route and multi-transaction step-04 loop at both named frontiers.
4. **Adversarial polarity** (`…-missing-signature-adversarial.test.ts`):
   an honest commitment (witness present), against which every road to a
   wrongful conviction is attempted and refused **in both planes** —
   offchain pre-check (message regex) and on-chain via raw
   guard-bypassing builders + `expectOnchainRefusalV1` (the
   `/failed script execution/` guard keeps the negatives honest) — each
   with a control transaction differing only in the attacked bit that
   lands. Scenarios, each mapped to its onchain selector (§1):
   - direction refusal: honest witness present → step-04 fold refusal at
     `required_signature_is_present == False`;
   - substituted signer list at step-02 (door authenticate-once);
   - out-of-range signer ordinal (abort-never-clamp);
   - one-sided forged witness set (`addr_tx_wits_hash` forged);
   - **the both-sides slashing forgery** — genuine body ‖ empty
     witness-set tail with empty preimage, and the substituted-set
     variant — refused by the thread-anchored
     `verified_witness_set_hash`;
   - tier-3 decoy carriage (decoy witness vector under a certificate) —
     refused by the welded-hash equality;
   - wrong vkey at step-03 (hash mismatch);
   - a third party driving or cancelling another prover's thread
     (`outsider` wallet; validator demands the named prover's signature).
5. **Negatives and resume** (`…-missing-signature-negatives.test.ts`):
   cancel at each of the four steps with re-init after every abort; resume
   from every position after a simulated crash (locate by asset name,
   drive to mint); advance a multi-batch subject to an interior step-04
   checkpoint and prove both deterministic resume-to-mint and cancellation;
   the fail-closed refusals a well-formed thread can walk into
   (stale/foreign opening evidence and uncommitted checkpoint bytes).

### 8.3 Test-support extensions (extend, do not fork)

- `tests/support/emulator/contracts.ts`: `buildMissingSignatureChainV1`
  (backwards application per §1's table; both cert-policy parameters from
  `base.fieldPreimageCertificate.policyId` — the always-succeeds stand-in
  per #579 ruling A; production applies the real policy), a
  `realMissingSignature` option-bag flag, record assembly sharing the
  double-spend computation-thread/fraud-proof pair (decoding precedent).
- `tests/support/emulator/harness.ts`:
  `MISSING_SIGNATURE_TEST_CATEGORY_ID_V1 = "0000000e"` + a fourth
  `extraCategories` spread.
- `tests/support/emulator/removal-deployment.ts`:
  `MISSING_SIGNATURE_REMOVAL_DEPLOYMENT_ENTRY_V1 =
"fraudProofMissingSignature"` + conditional manifest spread.
  **`src/remove-fraudulent-block.ts` needs zero changes.**
- `tests/support/emulator/native-tx.ts`: **the one blocking gap** —
  `makeNativeTx` hard-codes `requiredSignersPreimageCbor:
EMPTY_CBOR_LIST` (`:50`) and takes only `addrTxWitsPreimageCbor`. Widen
  with optional `requiredSignerHashes` / witness-set knobs, defaults
  preserving current behavior (existing consumers:
  `field-opening-v1.test.ts` only).
- New `tests/support/missing-signature-emulator-v1.ts`: scenario setup
  (committed block with the faulty accepted tx; honest variant with the
  witness present; fat-witness-set variant for the carriage route), a
  `publishMissingSignatureReferenceScriptsV1` leg publishing the four
  steps as reference scripts (§2.3, the `publishDecodingReferenceScriptsV1`
  shape), prover deps wiring, raw guard-bypassing step/cancel builders. Hoist
  `expectOnchainRefusalV1` from
  `tests/support/native-script-decoding-emulator-v1.ts:1476` into a shared
  `tests/support/emulator/` module with a re-export at its old site (no
  call-site churn) — two families now need it (§10 D7).

### 8.4 What lands at registration

The same re-pin table as the decoding plan §8.3 (catalogue roots, watcher
catalogue pins, blueprint validator counts, node catalogue tail,
inspect-contracts unions/manifest identity), executed by the registration
wave through the recorded derivation routes, never hand edits.

---

## 9. Sequencing and dependencies

1. **Gate: the 2026-08-26 removal-leg working tree lands.** The explicit
   removal category seam and the emulator removal precedent this plan's
   suite 3 builds on are currently uncommitted (`remove-fraudulent-block.ts`,
   `removal-deployment.ts`, the decoding direction-a removal leg,
   `catalogue-status.md`). Nothing else gated on external work. The envelope
   escalation subsequently amended step-04 with the bounded walk recorded in
   §1; no further onchain amendment remains in flight for this wave.
2. **Suite 1** (envelope/frontier/ExUnits) exposed the whole-field step-04
   memory gap; the bounded absence-walk escalation landed and the suite now
   closes the 140-/318-witness and worst-depth step-01 gates.
3. **SDK schemas** (§4.1) + suite 2.
4. **Evidence + submitters + init fork + cancel** (§4.2), with the
   `makeNativeTx` widening and harness extensions (§8.3) landing alongside
   so each submitter is emulator-exercised as it lands.
5. **Proving core + adapters** (§4.3).
6. **Suites 3–5** green: full lifecycle through mint and removal, the
   adversarial battery, negatives/resume — zero pin movement.
7. **Detection wiring** (§3): classification + vkey recovery + finding
   emission in the watcher, journaling both refused classes.
8. **Docs:** `catalogue-status.md` row 8 → 🔶 "implemented and
   emulator-proven under the reserved id; production registration
   outstanding" (+ §5 buckets, §3 SDK-catalogue row), `coverage-matrix.md`
   §13 Q16 row, `offchain-reference.md` §3 workflow entry.
9. **Registration wave** (separate, owner-scheduled): id allocation
   (re-verify next-free; expected `0000000e`), the §2.2 appends, CLI
   verbs, watcher `families[]`, re-pins, fresh genesis-level deployment.

---

## 10. Decision register

Decided under the AGENTS.md north star (correctness, safety, liveness,
performance, convenience), following the owner's 2026-08-25 delegation
precedent for the decoding register; each entry records what would reopen
it.

- **D1 — Category id: `0000000e` expected, written only at registration.**
  Grep-verified unclaimed after the three standing reservations. Reopens
  only if another family reserves it first; the registration wave
  re-verifies regardless.
- **D2 — Deployment shape: reference scripts (OWNER RULING 2026-08-26).**
  Fault-proof step validators always deploy as reference scripts and are
  referenced, never attached inline — regardless of compiled size. This
  family joins the decoding family's `referenceScriptTargets` family-steps
  class; the measured sizes (§2.3) inform frontier arithmetic only. Not a
  delegated decision; not reopenable by measurement.
- **D3 — Step-01 carriage: bare `NativeTxInclusionArgs` only.** That remains
  the only arm on-chain. The real worst-depth reference-script transaction
  passes byte and ExUnits limits, so no subject-size gap was demonstrated.
  Reopens only if the protocol's admissible subject domain or limits change.
- **D4 — Unknown-preimage corner: refused at classification (§7.2).** The
  interactive machine covers the corner preimage-free; the single-party
  hash-compare variant is recorded as a possible future onchain amendment,
  not proposed. Reopens if garbage-hash commitments are observed in
  practice.
- **D5 — Detection rides the existing replay divergence.** No new standing
  sweep; the classification + finding emission is default-on (detection
  coverage is safety), with the same isolated kill-switch convention as
  the decoding sweep.
- **D6 — Present-but-invalid routes to `invalidSignature` (§7.3).** The
  classification table is total across the two signature families. Not
  reopenable — it mirrors the onchain fold's semantics.
- **D7 — `expectOnchainRefusalV1` is hoisted to shared emulator support**
  with a re-export at its current site. Two families now depend on the
  `/failed script execution/` honesty guard; duplication would let the two
  copies drift.

---

## 11. Out of scope

- Any further onchain change beyond the completed bounded step-04 absence walk,
  lib twin, validator, and rebuilt blueprint recorded here. Production pins
  move only in the registration wave.
- The registration wave's execution (§2.2, §8.4, §9 step 9) — planned
  here, executed separately; no CLI verbs, no SDK catalogue append, no
  `submit-init.ts` union change until then.
- The `missing-native-script-tx` (Q17) and `withdrawn-reference-input`
  (Q19) siblings — Q17 in particular will reuse this family's harness
  extensions and should be planned against this document once it lands.
- The wrongful-rejection direction (§3.2) — `l2-tx-mistag` /
  `validationTraceDispute` territory.
- The step-03/04 hash-compare redesign (§7.2) and slashing economics.
- Enabling autonomous proving in any deployed watcher (adapter ships
  default OFF, decoding plan §10 Q5 conventions inherited).
- GOAL_PROGRESS ledger rows (owner may want one for this plan's landing).
