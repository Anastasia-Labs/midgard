# Committed-field-shape fault: offchain implementation plan (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`).
Catalogue row: `catalogue-status.md` §1 row 20. Normative rule:
`docs/spec/midgard-tx.md` §12.8 over §7.4 (fixed-stride arithmetic) and §5.4
(the 32,768-byte per-field aggregate bound). Unlike its sibling plans this
document ships **with** its implementation in the same working tree: the
family modules, emulator support and both-polarity suites described in §4
and §8 exist as uncommitted files beside it, so every "will" below is also a
"does" that the suite run in §8.5 witnesses.

The parity bar is `missing-signature-offchain-plan-v1.md` and, transitively,
the `native-script-decoding` family as built on this branch: explicit
pre-registration contracts records, per-step submitters plus cancel, a
reserved test category id wired only through the emulator harness, and
lucid-evolution emulator suites in both polarities — through fraud-proof
mint **and** fraudulent-commitment removal. Everything those plans decided
generically is inherited here, not re-decided.

Standing rulings this plan implements and never re-opens:

- **Reference scripts always (owner ruling 2026-08-26):** fault-proof step
  validators deploy as reference scripts and are referenced, never attached
  inline, regardless of compiled size (§2.3, §10 D2).
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through the full lifecycle, and an adversarial prover against
  an honest commitment is refused **on-chain at the exact check**, not
  merely by offchain guards.
- **Pre-registration explicit-record discipline:** the family's id never
  routes through the deployment manifest — `parseFraudProofCatalogueDeploymentInfo`
  silently drops non-canonical keys (`catalogue-status.md` §3). No CLI verb
  in `bin.ts`, no entry in `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, no
  `submit-init.ts` category-union change until the registration wave.
- **Reserved ids are expected, not promised:** `00000012` (index 18) is the
  assigned test-harness id; the production id is written only by the
  registration wave, which re-verifies "next free after standing
  reservations" at allocation time.
- **Removal via explicit category:** `remove-fraudulent-block.ts`'s
  `RemoveFraudulentBlockExplicitCategory` seam drives removal with every
  fail-closed check intact and zero changes to that module. The fraud-proof
  token is permanent by design; the state-queue node NFT burns.
- **Cancellation is an explicit prover decision:** no submitter cancels on
  its own.

All `file:line` anchors are against HEAD `a1724e63` plus this working tree.

---

## 1. The contract the builders must satisfy

The as-built onchain family
(`onchain/aiken/validators/fraud-proofs/committed-field-shape/step-0{1,2}.ak`,
rule in `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule.ak`)
is a **two-step** chain. Its offchain twin is
`demo/midgard-sdk/src/fraud-proof/committed-field-shape-v1.ts`, complete
with cross-language goldens; nothing onchain or SDK-side changes in this
wave.

### 1.1 Step 01 — bind, authenticate, forward the verdict

Parameters, in blueprint order: `step_02_validator_script_hash`,
`computation_thread_token_policy_id`, `hub_oracle`,
`field_preimage_certificate_policy_id`.

`ct.Continue(Args { inclusion, claim })` where:

- `inclusion : NativeTxInclusionCarriage` — the **existing** shared binding
  path (`pass_native_tx_to_next_step_carried`), redeemer-carried MPF
  membership or the #545 published-chunk transport. No new binding path is
  introduced (hard constraint); the native codec precondition runs inside
  it, so a miskeyed leaf is `da-hash-preimage`'s fault and never this
  family's.
- `claim : CommittedFieldClaimV1` — **§12.7's claim type, used rather than
  re-declared** (one accusation, one wire spelling): `BodyFieldClaim
{ field_index, carriage }` for slots 0–5, `WitnessFieldClaim
{ field_index, witness_set, carriage }` for slots 6–8, carriage one of
  `Inline`/`RawUtxo`/`Certified` (§8 tiers 1–3).

The step authenticates the claimed slot's preimage against the §4
commitment positionally extracted from the block-committed compact
structure (`authenticated_committed_preimage` — the non-aborting door
entry), computes `committed_field_shape_verdict_v1(field_index, preimage)`,
and requires the thread output at exactly the parameterized step-02 hash
carrying `step_02.State { bad_tx_id, field_index, verdict }`. A fabricated
verdict or re-addressed field index cannot be forwarded — both members are
recomputed on-chain.

Verdict code space (SDK twin constants in `committed-field-shape-v1.ts`):
`0` admissible, `1` not_an_envelope (**§12.7's fault — non-convicting
here**), `2` field_byte_bound (§5.4), `3` wrong_stride (§7.4). Byte bound
is checked before stride, mirroring the door's own refusal order (§12.1
one-spelling).

### 1.2 Step 02 — adjudicate and finalize

Parameters, in blueprint order: `fraud_proof_token_policy_id`,
`fraud_proof_token_address`, `computation_thread_token_policy_id`.

`finalize` (shared machinery: burn thread NFT, mint the permanent
fraud-proof token at the always-fails fraud-proof address, same asset name
`categoryId ‖ headerHash`) plus the family predicate
`is_committed_field_shape_violation_v1(field_index, verdict)`: convicts
exactly `verdict ∈ {2, 3}` with `0 ≤ field_index < 9`. `admissible` is the
honest block; `not_an_envelope` is the §12.7 disjointness boundary — one
committed field must never finalize under two fault kinds.

### 1.3 The evidence model, per step

| Step | Evidence in                                                                            | State out (thread datum `data`)         |
| ---- | -------------------------------------------------------------------------------------- | --------------------------------------- |
| init | catalogue membership proof of `(id, step01)`                                           | `null` (initial datum)                  |
| 01   | tx inclusion (root, proof, compact CBOR) + claim (slot, carriage bytes[, witness set]) | `{ bad_tx_id, field_index, verdict }`   |
| 02   | none — adjudicates the pinned triple                                                   | thread burned; fraud-proof token minted |

The offchain evidence record is the SDK's
`CommittedFieldShapeEvidenceV1` (verdict, stride, byte count, isViolation),
built by `committedFieldShapeEvidenceFromCommittedFieldV1` and re-derived
fail-closed at every submitter boundary, so the offchain plane can never
build a transaction the L1 verdict recomputation would contradict.

---

## 2. Registration

### 2.1 Category id

Reserved **test** id: `00000012` (= `categoryId(18)`), assigned to this
family by the parent orchestrator; ids `0000000b`/`0000000c` (fabricated
families), `0000000d` (native-script-decoding) and `0000000e`
(missing-signature plan) precede it, and sibling waves running concurrently
hold `0000000f`–`00000011`. The constant lives in
`tests/support/emulator/harness.ts` as
`COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1` with the standing
expected-but-not-promised caveat, and is wired **only** through the
`buildCatalogueDeploymentInfo` extra-categories sidecar (base roots and
proofs stay byte-identical when the family is off).

### 2.2 What registration (later, parent-owned) touches — and this wave does not

`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` + `FraudProofs` record in
`demo/midgard-sdk/src/fraud-proof/catalogue.ts`, an SDK
`buildCommittedFieldShapeFaultProofContracts` chain builder, the
`submit-init.ts` category union, `bin.ts` verbs, and the deployment
manifest key. This wave touches none of them.

### 2.3 Script deployment: reference scripts

Both steps publish as plain reference-script UTxOs
(`publishPlainReferenceScriptUtxo`, `oversized: true` — uniform with the
decoding family), and every spending submitter requires a published UTxO and
verifies that it hashes to the step being spent before building anything
(`requireCommittedFieldShapeReferenceScriptV1`). There is no inline-validator
fallback.

---

## 3. Detection

### 3.1 What exists

The SDK rule module is complete: total verdict over arbitrary bytes at all
nine slots, `MIDGARD_FIXED_STRIDE_FIELD_INDICES_V1` derived (not
transcribed) from the shared stride table — slots 0/1 stride 40, 3/4
stride 30, 7 stride 103; slots 2/5/6/8 walk-derived (stride check does not
apply).

### 3.2 Recognition and classification

`prepare-committed-field-shape-v1.ts` (new, §4.2) classifies a candidate
transaction from its **canonical** form: all six body preimages and all
three witness preimages are in `MidgardNativeTxCanonicalV1`, so
`classifyCommittedFieldShapeFieldsV1` renders all nine slot verdicts in one
pass and `prepareCommittedFieldShapeFromCanonicalTxV1` picks the accused
slot (caller-pinned or first violating), builds the evidence record, the
`CommittedFieldClaimV1` (witness slots get the derived
`NativeTxWitnessSetCompact`), and the exact step-02 state — refusing, with
named codes, a slot whose verdict is non-convicting. The security-grade
DA-first entry point (authenticated header observation + retained-DA
payload → canonical tx bytes) is deferred to the watcher-integration wave
(§10 D5): the da-hash-preimage prepare pipeline already owns
payload-to-leaf authentication, and this family plugs in behind it once the
per-transaction canonical bytes route is shared.

### 3.3 The carriage residue (spec-normative, recorded here on purpose)

§12.8's own residue paragraph: a committed preimage **above 32,768 bytes**
is convictable by the rule, but unreachable in carriage — tiers 1/2 are
bounded by the L1 transaction envelope and the tier-3 certificate policy
refuses `total_length` above the §5.4 bound. So an end-to-end L1 conviction
can only ever exercise `wrong_stride` (code 3); the `field_byte_bound`
conviction (code 2) is proven at rule level (SDK + Aiken goldens) and at
step level (the Aiken step-01/step-02 selectors drive it through the
validators with fixture carriage). The emulator suites therefore convict
via wrong-stride and do **not** fake a byte-bound route L1 could never
accept. This is spec design, not a gap this wave may close.

---

## 4. New offchain modules

### 4.1 SDK

None. `committed-field-shape-v1.ts` is complete and untouched.

### 4.2 Family modules (`demo/midgard-fault-proofs/src/committed-field-shape/`)

| Module                                    | Role                                                                                                                                                                                                                                                                                                   |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `contracts-v1.ts`                         | Blueprint titles (`fraud_proofs/committed_field_shape/step_0{1,2}.main.spend`), pinned parameter order, `CommittedFieldShapeContractsV1` explicit record (two steps, shared computation-thread/fraud-proof pair, hub-oracle/state-queue/certificate policies). Deliberately **no** `categoryId` field. |
| `submit-common-v1.ts`                     | `CommittedFieldShapeCatalogueCategoryV1`, submit error/labels, thread-UTxO + NFT validation at a step, reference-script validation, fail-closed step-state reader. Mirrors the decoding family's `submit-common-v1.ts`.                                                                                |
| `prepare-committed-field-shape-v1.ts`     | §3.2 classification and proof-plan builder over canonical transactions; nine-slot verdict table; claim + step-02 state emission; named rejection codes.                                                                                                                                                |
| `submit-committed-field-shape-init.ts`    | Pre-registration init fork (decoding pattern): explicit contracts + category + catalogue triple, category-vs-step-01-hash guard, `Init` mint with catalogue membership withdrawal.                                                                                                                     |
| `submit-committed-field-shape-step-01.ts` | Binds the accused transaction (redeemer-carried or published-chunk inclusion), re-derives the verdict locally and **refuses non-convicting claims before paying**, encodes `Continue(Args { inclusion, claim })` with tier-1 `Inline` carriage, pays the thread to step 02 with the derived state.     |
| `submit-committed-field-shape-step-02.ts` | Reads the pinned triple back from the on-chain datum, re-checks `isCommittedFieldShapeViolationV1` fail-closed, burns the thread, mints the permanent token.                                                                                                                                           |
| `submit-committed-field-shape-cancel.ts`  | `ct.Cancel` at either step, prover-only, explicit decision.                                                                                                                                                                                                                                            |
| `index.ts`                                | Barrel; one `export *` line added to `src/index.ts`.                                                                                                                                                                                                                                                   |

### 4.3 Carriage tiers in the submitters

The step-01 submitter carries the claim preimage **tier-1 `Inline`** in
this wave: every reachable conviction shape that fits one redeemer
(wrong-stride envelopes are typically tiny — the canonical fixture is six
bytes) takes tier 1, and the on-chain door's tier-2/tier-3 admission is
already proven by the Aiken step-01 selectors
(`binds_a_raw_utxo_carriage`, `binds_a_certified_carriage`). Tier-2/3
claim carriage in the TypeScript submitter is deferred with the DA-first
entry point (§10 D6) — the carriage schemas are shared with §12.7, so the
sibling canonical-decodability wave's tier work lands for both.

---

## 5. Emulator scenario construction

The scenario is real, not mutated: `makeNativeTx({ spendInputCbors:
[fourByteBuffer], fee })` materializes a **canonical-grammar-valid** native
transaction (`validateMidgardNativeTxCanonicalV1` checks §5.1 grammar per
field, not strides) whose slot-0 committed preimage is `81 44 de ad be ef`
— six bytes where slot 0's arithmetic demands `1 + 40·N`. The block then
commits it exactly as an operator would:

1. `computeMidgardNativeTxIdV1(tx)` → leaf key;
   `encodeMidgardNativeTxCompactV1(tx.compact)` → leaf value; single-leaf
   raw MPF via `@aiken-lang/merkle-patricia-forestry`.
2. `setupFraudulentBlockV1({ fixture: { transactionsRoot, l2TransactionCount: 1n } })`
   commits the counted root into a state-queue header signed by the funder
   (the operator to be slashed).
3. The claim: `BodyFieldClaim { field_index: 0, carriage: Inline
{ preimage: "8144deadbeef" } }`; the door hashes it against the
   compact's `spend_inputs_hash` and it authenticates because the operator
   really committed those bytes.

Honest control: the same construction with one 38-byte spend-input item
(preimage `81 5826 …` = 41 = 1 + 40·1) — shape-admissible at slot 0, used
by every negative. Non-envelope control (`miscountedMidgardFieldPreimageV1`
analog `8041` committed at a variable-width slot) exercises the §12.7
disjointness boundary end-to-end.

---

## 6. Economics and pacing

Inherited unchanged from the decoding plan: two thread transactions plus
init plus removal, thread min-ADA reclaimed at finalize into the token
UTxO, cancel reclaims to the prover. No family-specific pacing knobs — the
chain is two steps and each transaction is small (the claim preimage is
tier-1 bytes). Reference-script publication cost is borne once per
deployment, as everywhere else.

---

## 7. Cancel, recovery, and the corners

- **Crash-resume:** every submitter is idempotent against the chain state —
  it locates the thread by out-ref, validates step address + NFT + datum,
  and refuses anything mid-flight it did not expect. Re-running a completed
  step fails locally (thread no longer at that address).
- **Cancel:** available at both steps, prover-signature-gated on-chain;
  the cancel submitter refuses any other signer up front. Exercised in the
  emulator negatives, including the third-party refusal.
- **The unreachable byte-bound corner:** recorded in §3.3; refused at
  classification time only in the sense that no carriage can deliver it —
  the prepare module still classifies it correctly (verdict 2) and the
  refusal surfaces when carriage selection finds no admissible tier.
- **Slot/claim-kind mismatches:** a body claim at a witness slot (and vice
  versa) is refused by the prepare module and, independently, by the
  on-chain door (Aiken selectors `rejects_a_body_claim_at_a_witness_field`
  / `rejects_a_witness_claim_at_a_body_field`).

---

## 8. Testing

### 8.1 Corner-case coverage of the §7.4/§5.4 constraint space

The constraint space is `(slot, shape-rule)`; representative cells are
driven end-to-end in the emulator and every remaining cell is pinned at a
cheaper tier that already exists:

| Cell                                             | Where proven                                                                                                                                                                     |
| ------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| slot 0 wrong-stride (body, stride 40)            | **Emulator e2e** (lifecycle: init → 01 → 02 → removal)                                                                                                                           |
| slot 7 wrong-stride (witness, stride 103)        | Aiken step-01/02 selectors + SDK goldens; witness-claim plumbing typechecked in the prepare module suite of checks (classification renders slot 7 from `addrTxWitsPreimageCbor`) |
| slots 1/3/4 wrong-stride (stride 40/30/30)       | SDK↔Aiken golden vectors over the shared stride table (`MIDGARD_FIXED_STRIDE_FIELD_INDICES_V1` is derived, so a table drift breaks the goldens, not just one slot)              |
| byte-bound at variable-width slots (2/5/6/8)     | Aiken step selectors (`binds_an_oversize_body_field`, step-02 `convicts_an_oversize_field`) + rule goldens; e2e-unreachable by §12.8's carriage residue (§3.3)                   |
| byte-bound at fixed-stride slots (checked first) | rule-level order pin (SDK + Aiken `rule.ak`)                                                                                                                                     |
| admissible at every slot                         | honest-control emulator negative (slot 0) + Aiken valid-block selectors                                                                                                          |
| non-envelope (§12.7's) at any slot               | **Emulator e2e negative** (step-01 binds forwarding verdict 1, step-02 refused at the predicate) + Aiken disjointness selectors                                                  |
| `field_index ∉ [0,9)` / unknown verdict code     | Aiken step-02 refusals + SDK predicate tests                                                                                                                                     |

### 8.2 Suites (new per-family test files, vitest per-file isolation)

1. **`tests/submit-init-emulator-committed-field-shape.test.ts`** — the
   real-fault polarity: wrong-stride slot-0 block committed on the
   emulator; reference scripts published; init (family fork, id
   `00000012`) → step-01 (asserts the on-chain step-02 datum equals the
   SDK-derived `{ bad_tx_id, field_index: 0, verdict: 3 }`) → step-02
   (permanent token minted, thread burned at both step addresses) →
   explicit-category removal (state-queue node NFT burned, operator
   slashed, scheduler rewound, fraud-proof token retained at the same
   out-ref, second removal refused).
2. **`tests/submit-init-emulator-committed-field-shape-adversarial.test.ts`**
   — the adversarial polarity and negative controls, against an **honest**
   commitment:
   - offchain fail-closed: the honest step-01 submitter refuses an
     admissible slot (named error) before building anything;
   - **on-chain at the exact shape adjudication:** a raw guard-bypassing
     step-01 forwarding a fabricated `wrong_stride` verdict for the honest
     bytes is refused by the validator's output-state recomputation
     (`/failed script execution/`);
   - uncommitted bytes: a raw step-01 claiming a wrong-stride preimage the
     block never committed is refused by the §4 hash check inside the door;
   - **disjointness e2e:** a committed non-envelope binds legitimately
     through step-01 (verdict 1 forwarded) and the raw step-02 finalize is
     refused at `is_committed_field_shape_violation_v1`;
   - cancel: the prover cancels and reclaims; a funded outsider's raw
     cancel is refused on-chain at the signature demand.

### 8.3 Test-support extensions (extend, do not fork)

- `tests/support/emulator/contracts.ts`: `buildCommittedFieldShapeChainV1`
  (step 02 applied first, blueprint order pinned) + a
  `realCommittedFieldShape` flag + the explicit record sharing the
  double-spend family's thread/fraud-proof policies and the always-succeeds
  field-preimage-certificate stub (#579 ruling A).
- `tests/support/emulator/harness.ts`:
  `COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1 = "00000012"` + the
  extra-categories spread keyed off `contracts.committedFieldShape`.
- `tests/support/emulator/removal-deployment.ts`:
  `COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY_V1 =
"fraudProofCommittedFieldShape"` + the conditional manifest spread.
- `tests/support/committed-field-shape-emulator-v1.ts` (new): family
  harness (+ outsider wallet), scenario builders (wrong-stride / honest /
  non-envelope committed blocks), reference-script publication, the
  explicit removal-category record, raw step-01/step-02/cancel builders,
  and the shared on-chain-refusal assertion.

All shared-file changes are additive only (a sibling wave owns the
canonical-decodability seams in the same files).

### 8.4 What lands at registration

Production id allocation, SDK chain builder + catalogue row, CLI verbs,
`submit-init` union, watcher routing, DA-first prepare entry point, tier-2/3
submitter carriage. Out of scope here (§11).

### 8.5 Commands

From `demo/midgard-fault-proofs/` (blueprint regenerated into
`onchain/aiken/plutus.json` with the repo's patched aiken fork first):

```
pnpm vitest run tests/submit-init-emulator-committed-field-shape.test.ts
pnpm vitest run tests/submit-init-emulator-committed-field-shape-adversarial.test.ts
pnpm typecheck && pnpm lint
```

Aiken side (untouched, sanity only):
`aiken check -m 'committed_field_shape/'` (trailing slash — the bare filter
is vacuous).

---

## 9. Sequencing and dependencies

1. Blueprint regeneration (patched fork) — done; both step titles present.
2. Family src modules (§4.2) — depend only on existing SDK exports.
3. Test-support extensions (§8.3) — additive, after (2).
4. Suites (§8.2) — after (3).
5. `catalogue-status.md` row 20 minimal-diff update — last, reflecting the
   green run.

---

## 10. Decision register

- **D1 — id `00000012` accepted as assigned;** wired through the harness
  sidecar only (standing discipline).
- **D2 — reference scripts for both steps** (owner ruling; no size
  exception sought).
- **D3 — emulator conviction is wrong-stride only.** The byte-bound
  conviction is rule/step-level covered; §12.8's carriage residue makes an
  e2e byte-bound route impossible by design, and faking one (e.g. an
  emulator with inflated limits) would prove a transaction L1 refuses.
  Recorded rather than escalated: the spec text itself pins this.
- **D4 — the claim type is §12.7's `CommittedFieldClaimV1`,** imported from
  `canonical-decodability-v1.ts` (SDK already enforces the one-spelling
  rule); the sibling wave is not touched.
- **D5 — DA-first prepare entry point deferred** to watcher integration;
  the core builder is pure over canonical transactions and names its
  refusals, exactly like `prepareDaHashPreimageFromCommittedLeavesV1`'s
  core/security split.
- **D6 — submitter claim carriage is tier-1 `Inline` this wave;** tiers 2/3
  are on-chain-proven and shared with §12.7's carriage work (§4.3).
- **D7 — removal drives the explicit-category seam** with a family
  manifest-entry name (`fraudProofCommittedFieldShape`) that exists only in
  the emulator manifests until registration.

## 11. Out of scope

Catalogue registration and production id; `bin.ts`/`submit-init.ts`/SDK
catalogue surfaces; watcher scan/routing integration; DA-first evidence
fetch; tier-2/3 TypeScript claim carriage; any onchain change (none was
needed — the family's validators and goldens are REAL and untouched); the
sibling canonical-decodability (§12.7) wave.
