# Canonical-decodability fault: offchain implementation plan (v1)

> **Registration update (2026-08-26):** this family is now registered as
> `canonicalDecodability` at `00000011`. Generic Init, catalogue/inspection,
> node/core deployment identity, watcher proof-thread topology, and both
> mandatory authenticated reference scripts are wired. Family-specific CLI,
> DA-first watcher detection/prover mounting, preprod, and live evidence remain
> open. The identity change requires fresh genesis/redeployment; there is no
> migration or compatibility path.

> Status: implemented alongside this plan (same working tree). The on-chain
> family (`docs/spec/midgard-tx.md` §12.7, two steps) and its SDK module
> (`demo/midgard-sdk/src/fraud-proof/canonical-decodability-v1.ts`) predate
> this plan and are consumed unchanged. What this plan adds is the offchain
> prepare/submit chain, canonical category wiring, and both-polarity emulator
> end-to-ends through fraud-proof-token mint AND fraudulent-commitment
> removal, at the same bar as
> `docs/fault-proofs/missing-signature-offchain-plan-v1.md`.

The fault, in one paragraph. Under §4 an operator commits
`blake2b_256(preimage_i)` for each of §2.5's nine committed fields, and §5.1
says what a preimage may be: a minimal-width definite array header followed by
exactly `N` minimal-width byte-string-wrapped items, ending exactly at the
preimage's last byte. An operator who commits anything else has committed a
field no reader can open — every §8.8 view door settles §5.1 with `expect`, so
the field aborts every consumer, including the `CanonicalDecode` phase whose
job is to render a verdict about it. The dispute stalls instead of rejecting:
an operator escape hatch. The owner ruling of 2026-08-11 closes it by direct
fault. Step 01 binds the disputed transaction to the block's counted
`transactions_root` (native codec precondition ON — a mis-keyed leaf is
`da-hash-preimage`'s fault, not this one's), authenticates the carried
preimage bytes against the §4 commitment positionally extracted from the
committed compact structure, and forwards the derived
`(bad_tx_id, field_index, verdict)`; step 02 finalizes iff
`0 <= field_index < 9`, `0 <= verdict < 11`, and `verdict != 0`
(`verdict_grammatical`). No preimage bytes travel between steps. An honest
block _binds_ (verdict 0) but can never finalize — the family's
honest-operator safety asymmetry, pinned on-chain by
`canonical_decodability_step_01_binds_a_grammatical_field_without_convicting`
and `canonical_decodability_step_02_rejects_a_grammatical_field`.

## 1. The contract the builders must satisfy

Two validators, already real and golden-tested (88/88 at this tree):

| Validator | Blueprint title                                          | Declared parameters (order is wire)                                                                                         |
| --------- | -------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| step 01   | `fraud_proofs/canonical_decodability/step_01.main.spend` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`, `field_preimage_certificate_policy_id` |
| step 02   | `fraud_proofs/canonical_decodability/step_02.main.spend` | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id`                            |

The chain is applied backwards (step 02 first) because step 01 is
parameterized by step 02's script hash. Both steps share the double-spend
family's computation-thread and fraud-proof policies, like every other family.

Step 01's `Continue` redeemer is `Args { inclusion, claim }`:

- `inclusion: NativeTxInclusionCarriage` — the shared native binding,
  **exclusively** through `pass_native_tx_to_next_step_carried`
  (`common.ak`), which dispatches `RedeemerCarriedInclusion` to
  `pass_native_tx_to_next_step` or takes the #545 published-chunk transport.
  No new binding path exists or is added anywhere in this work
  (`catalogue-status.md` §1a).
- `claim: CommittedFieldClaimV1` — `BodyFieldClaim { field_index, carriage }`
  (Constr 0, fields 0–5) or
  `WitnessFieldClaim { field_index, witness_set, carriage }` (Constr 1,
  fields 6–8; `witness_set` is checked against the committed
  `witness_set_hash` inside the door). `carriage: FieldCarriageV1` is §8's
  ladder: `Inline` / `RawUtxo` / `Certified` — all three tiers admissible at
  all nine fields (§12.7 normative condition).

Step 01's obligations, all recomputed on-chain: output at
`step_02_validator_script_hash` exactly; output state equals
`step_02.State { bad_tx_id, field_index, verdict }` where the pair
`(field_index, verdict)` is derived by `committed_field_verdict_v1` — the
door's §4 hash check (`blake2b_256(carried bytes) == positional commitment`)
followed by the total `envelope_verdict_v1`. A fabricated verdict, a
re-addressed field index, uncommitted bytes, a claim half at the wrong §2.5
side, or a foreign next-step hash all abort (each has a named Aiken selector).

Step 02's obligations: `common.finalize` (thread burn + permanent fraud-proof
token mint at the fraud-proof address) plus
`is_canonical_decodability_violation_v1(field_index, verdict)` — bounds as
refusals, then `verdict != 0`.

Field-index table the offchain twin must agree with
(`field_commitment_at`, `native-tx-field-access-v1.ak`):

| index | committed field    | compact slot                        |
| ----- | ------------------ | ----------------------------------- |
| 0     | spend inputs       | `body.spend_inputs_hash`            |
| 1     | reference inputs   | `body.reference_inputs_hash`        |
| 2     | outputs            | `body.outputs_hash`                 |
| 3     | required observers | `body.required_observers_hash`      |
| 4     | required signers   | `body.required_signers_hash`        |
| 5     | mint               | `body.mint_hash`                    |
| 6     | script witnesses   | `witness_set.script_tx_wits_hash`   |
| 7     | address witnesses  | `witness_set.addr_tx_wits_hash`     |
| 8     | redeemer witnesses | `witness_set.redeemer_tx_wits_hash` |

Note the 6/7 order: index 6 is the **script** witness hash even though the
compact witness-set struct declares `addr_tx_wits_hash` first. The SDK schemas
(`NativeTxWitnessSetCompactSchema`) carry the struct order; the index table is
the door's and both sides already agree (SDK
`canonical-decodability-v1.ts` mirrors the verdict constants and claim wire
by name; blueprint parity rows 15–16 in
`tests/support/emulator/blueprints.ts` cover the two wire types).

## 2. Registration

### 2.1 Category id

Canonical category id: **`00000011`**. The production catalogue order,
generic `submitInit` category union, deployment manifests, inspection, and
watcher proof-thread authority bind that id to this family's step-01 hash.
Every module below still takes the id as data so the proof logic is not coupled
to a test constant.

### 2.2 Registered deployment surface

`"canonicalDecodability"` is appended to the category order, generic
`submitInit` accepts it, and deployment-manifest entries publish both steps.
The contracts record deliberately has **no** `categoryId` member, and every
submitter takes the category explicitly. Family-specific CLI verbs and watcher
detection/prover mounting remain separate operational work.

### 2.3 Script deployment: reference scripts (owner ruling 2026-08-26)

Both steps deploy as **reference scripts, never inline-attached** — the owner
ruling is unconditional and size-independent (the legacy inline families are
not precedent). The emulator suites publish both step validators via
`publishPlainReferenceScriptUtxo({ oversized: true })`
(`publishCanonicalDecodabilityReferenceScriptsV1`) and drive every step spend
through `readFrom`. The submitters accept the published reference-script UTxO,
verify the carried script hashes to the very step being spent before building
anything (`requireCanonicalDecodabilityReferenceScriptV1`). The public
submitters require that UTxO; there is no inline-attachment fallback.

## 3. Detection

What exists upstream: the watcher/DA pipeline already yields, per committed
block, the raw transactions MPF and each leaf's compact structure. Detection
for this family is a pure scan over committed compacts:

1. For each leaf whose key equals its derived native tx id (codec
   precondition — leaves failing it are `da-hash-preimage`'s), obtain the
   nine committed field preimages from the retained DA payload.
2. For each field, check `blake2b_256(preimage) == positional commitment`
   (DA integrity; a mismatch is a DA-layer fault, out of scope here) and run
   `midgardEnvelopeVerdictV1` over the committed bytes.
3. A non-zero verdict at any field is this family's fault. Record
   `(headerHash, badTxId, fieldIndex, committedPreimage, verdict)`.

The prepare module below is the pure core of step 3 with fail-closed
re-derivation; wiring it to the watcher's block feed is registration-wave
work (§9). The detection cannot false-positive against an honest operator:
the honest producer (`encode_field_preimage` / SDK `encodeCbor` of item
lists) cannot emit a non-envelope, and even a mistaken accusation dies at
step 02's `verdict != 0` after binding harmlessly.

## 4. New offchain modules

### 4.1 SDK

None. `canonical-decodability-v1.ts` is complete: verdict constants 0–10 and
`MIDGARD_ENVELOPE_VERDICT_CODE_COUNT_V1 = 11`, the total
`midgardEnvelopeVerdictV1` walk (asserted against the Aiken twin by the
cross-language golden vectors), `isCanonicalDecodabilityViolationV1`,
`miscountedMidgardFieldPreimageV1`, evidence builder, and all wire schemas
(claim enum, step-01 args/redeemer, step-02 state/datum/args/redeemer,
cancel). This plan builds on it and duplicates nothing.

### 4.2 Family modules (`demo/midgard-fault-proofs/src/canonical-decodability/`)

Same shape as `src/native-script-decoding/` (the family-module precedent),
sized to a two-step family:

| Module                                     | Owns                                                                                                                                                                                                                                                                                                                                                                                        |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `contracts-v1.ts`                          | `CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1`, `CanonicalDecodabilityContractsV1` (steps pair, shared thread/fraud-proof policies, hub-oracle/state-queue/certificate policy ids; **no** `categoryId`)                                                                                                                                                                                       |
| `prepare-canonical-decodability-v1.ts`     | pure fail-closed evidence core: decode + canonicity-check the committed compact, re-derive the tx id, extract the positional commitment for the accused index (witness fields re-derive `witness_set_hash` first), require the carried bytes to hash to it, compute the verdict, refuse `verdict == 0` (a valid block cannot be challenged) — returns evidence + wire claim + step-02 state |
| `submit-common-v1.ts`                      | thread-UTxO/step-datum/reference-script fail-closed helpers, explicit category record type (mirrors `NativeScriptDecodingCatalogueCategoryV1`)                                                                                                                                                                                                                                              |
| `submit-canonical-decodability-init.ts`    | thread `Init` mint under the explicit category (catalogue + hub-oracle + fraudulent-block reference inputs, PHAS membership withdrawal carrying the category proof)                                                                                                                                                                                                                         |
| `submit-canonical-decodability-step-01.ts` | bind + adjudicate-and-forward: re-runs the prepare core before building, then one transaction — thread spend with `Continue(Args { inclusion, claim })`, hub-oracle + state-queue reference inputs, PHAS membership withdrawal opening `(native_tx_id -> compact_cbor)` under the raw transactions root, thread paid to step 02 with the derived `State` inline                             |
| `submit-canonical-decodability-step-02.ts` | finalize: re-checks `isCanonicalDecodabilityViolationV1` against the on-chain datum, burns the thread, mints the permanent fraud-proof token at the fraud-proof address                                                                                                                                                                                                                     |
| `submit-canonical-decodability-cancel.ts`  | explicit prover cancellation at either step: locate/authenticate the thread, require that step's reference script, and burn through `BurnForCancellation`                                                                                                                                                                                                                                   |
| `index.ts`                                 | barrel, re-exported from `src/index.ts`                                                                                                                                                                                                                                                                                                                                                     |

Fail-closed convention (family-wide): every submitter re-derives everything it
asserts before paying for anything, and refuses locally with a typed message;
the on-chain check remains the authority and the emulator suites exercise
both planes separately.

### 4.3 Proving flow

Two steps need no §4.3-style multi-transaction proving core. The flow is
init → step 01 → step 02 → (independently) removal, each via its submitter;
crash-resume is positional (the thread NFT's address names the next
submitter, and each submitter validates the thread's location and datum
before proceeding). Cancellation is the shared `ct.Cancel` arm — an explicit
prover decision, never automatic. The family submitter is deliberately thin:
it adds positional step discovery, mandatory reference-script verification,
and prover authentication around that shared arm.

## 5. Carriage frontiers and envelope discipline

Two independent carriages ride step 01:

- **Inclusion carriage** (the leaf and its membership proof):
  `RedeemerCarriedInclusion` for proofs that fit the 16,384-byte envelope,
  `PublishedChunkInclusion` (#545) for deeper tries. Both are
  `pass_native_tx_to_next_step_carried`'s, inherited for free. The submitter
  implements both encodings through the shared machinery; the published-chunk
  transport remains emulator-proven end-to-end by the decoding family under
  the identical carriage type (D4).
- **Field carriage** (the accused committed bytes): §8's full ladder.
  `Inline` through the current 14,336-byte tier-1 frontier; `RawUtxo` through
  current `K = 15,148` bytes; `Certified` (tier 3) beyond `K` up to
  `max_transaction_aggregate_field_bytes`, with the §8.6 certificate token.
  The submitter implements `Inline`; the Aiken selector
  `canonical_decodability_step_01_binds_a_certified_carriage` pins tier 3 at
  15,200 bytes on-chain, and the tier-3 offchain publication helper
  (`publishFaultProofFieldCarriageV1`) already exists family-agnostically
  (D5).

Envelope discipline: the worst redeemer this family ships in the emulator
suites is a single-leaf membership proof plus an inline preimage of tens of
bytes — far inside the envelope. The cost ceiling of the on-chain walk is
priced in §12.7 (1,076-item ceiling); nothing offchain changes it.

## 6. Economics and pacing

Three transactions to token (init, step 01, step 02) plus one removal
transaction; one thread NFT of min-ADA riding through, returned into the
fraud-proof output at finalize. No per-step budget machinery is warranted at
this size (the decoding family's pacing policy exists for six-transaction
threads; a two-step thread is below the threshold where a budget gate earns
its complexity). The emulator has no settlement depth or maturity to observe.

## 7. Cancel, recovery, and the corners

### 7.1 Verdict-space corners (from §5.1's eleven codes)

The emulator suites pin the two structurally distinct violation shapes at
both §2.5 halves, and the Aiken layer already pins the rest:

- **body field, `verdict_trailing_bytes` (10)** — a miscounted envelope
  (`miscountedMidgardFieldPreimageV1(1, [..two items..])`) committed at
  field 2 (outputs): the walk finishes its declared item with bytes left.
- **body field, `verdict_missing_array_header` (1)** — the empty committed
  preimage, the one byte string shorter than §5.1's shortest form (`80`).
- **witness field 6, `verdict_missing_item_header` (5)** — a header
  declaring one item over an empty body, committed in the witness set;
  reachable only through the `witness_set_hash` re-derivation.
- **grammatical (0)** — the honest shape; binds, never finalizes (both
  planes, §8.2 suite 2).

The full 11-code space is pinned by `rule.test.ak` / `rule-golden.test.ak`
(65 tests) and the SDK twin's cross-language vectors; re-pinning every code
through L1 transactions would prove the emulator, not the family.

### 7.2 Crash-resume

Positional, as §4.3 notes. A thread abandoned mid-flight is re-entered by
reading the thread NFT's location; a thread at step 02 whose datum carries
verdict 0 (possible only via a bug, since step 01 recomputes) is refused by
both the step-02 submitter and the validator.

### 7.3 The aliasing corner (recorded; harmless by construction)

§4's field-hash aliasing (fields 0/1 and 3/4 commit identically for identical
content) does not weaken the family: aliased slots commit the _same bytes_,
so a non-envelope convicted under either index is a non-envelope under both
(rule.ak's module comment). No offchain guard is needed; the prepare core
simply reports the index the caller accused.

## 8. Testing

### 8.1 Where and how

`demo/midgard-fault-proofs/tests/`, vitest, Lucid emulator, real regenerated
blueprint (`aiken build --env testnet` with the pinned v1.1.23 toolchain;
`plutus.json` is gitignored and absent until built), `localUPLCEval: true`.
One new emulator scenario family per FILE (the `@lucid-evolution/uplc` wasm
heap is never reclaimed and vitest isolates per file — see
`tests/support/uplc-heap-guard.ts`). On-chain refusals are asserted through
`expectOnchainRefusalV1` (requires `/failed script execution/`), so a builder
error can never masquerade as a security pass; adversarial transactions are
built by raw guard-bypassing builders in the family's test-support module,
because every attack is one the honest submitters refuse locally first.

Both-polarity coverage is mandatory (owner directive 2026-08-25): the real
fault proves end-to-end through token mint AND fraudulent-commitment removal
(the state-queue node NFT burns; the fraud-proof token is permanent by
design and survives removal at the same out-ref), and the adversary against
an honest commitment is refused at the exact adjudication check on-chain.

### 8.2 Suites, in order of construction

1. **`submit-init-emulator-canonical-decodability.test.ts` — real-fault
   lifecycle.** A committed block whose outputs field (index 2) commits a
   miscounted §5.1 envelope. init → step 01 (bind + derive
   `verdict_trailing_bytes`) → step 02 (mint) → removal via the explicit
   category (`RemoveFraudulentBlockExplicitCategory`, reference scripts
   required): state-queue node NFT burned, root link emptied, operator
   slashed, scheduler rewound, fraud-proof token retained at the same
   out-ref, second removal claim finds nothing. Includes the step-02 datum
   equality pin against the offchain-derived state.
2. **`submit-init-emulator-canonical-decodability-adversarial.test.ts` —
   honest commitment, both planes.** An honest block (grammatical committed
   fields). Plane 1: the step-01 submitter refuses locally (`verdict 0`)
   before building. Plane 2 (raw builders): forwarding the truthful
   grammatical state through step 01 _binds_, and the step-02 finalize is
   refused by the validator at `is_canonical_decodability_violation_v1`
   (`failed script execution`); fabricating a violating verdict at step 01
   is refused by the state-equality recomputation; carrying bytes the block
   never committed is refused by the door's §4 hash check.
3. **`submit-init-emulator-canonical-decodability-witness-field.test.ts` —
   witness half + negative controls.** The witness-field scenario (field 6,
   `verdict_missing_item_header`) through mint, plus: a body claim at the
   witness index refused on-chain (claim-half assertion), and evidence
   manufactured under a transactions root the header never committed refused
   at the PHAS membership withdrawal (`failed script execution Withdraw`).

Every scenario asserts the thread NFT's exact custody at each hop
(`utxosAtWithUnit` emptiness on the vacated step address) and refusal
non-effects (thread untouched at the same out-ref, block still queued).

4. **`submit-init-emulator-canonical-decodability-cancel-resume.test.ts` —
   cancellation and positional recovery.** Cancels from both step addresses,
   then simulates a restart after step 01 by discarding the submitter result,
   rediscovering the thread NFT at step 02, and resuming through mint.
5. **`canonical-decodability-envelope-v1.test.ts` — deployment/carriage
   frontiers.** Pins both applied validators as distinct reference-publication
   targets, encodes both inclusion carriages, measures the shipped inline
   claim against the L1 envelope, pins the inline field frontier, and refuses
   a missing reference script.

### 8.3 Test-support extensions (extend, do not fork)

- `tests/support/emulator/contracts.ts`:
  `buildCanonicalDecodabilityChainV1` (step 02 then step 01, blueprint
  parameter order of §1) + `realCanonicalDecodability` option +
  `canonicalDecodability` contracts record (always-succeeds
  field-preimage-certificate stub per the #579 ruling-A seam, same as
  decoding).
- `tests/support/emulator/harness.ts`:
  `CANONICAL_DECODABILITY_TEST_CATEGORY_ID_V1 = "00000011"` + the
  extra-category spread into `buildCatalogueDeploymentInfo`.
- `tests/support/emulator/removal-deployment.ts`:
  `CANONICAL_DECODABILITY_REMOVAL_DEPLOYMENT_ENTRY_V1 =
"fraudProofCanonicalDecodability"` + the conditional manifest spread.
- `tests/support/canonical-decodability-emulator-v1.ts` (new): harness maker,
  committed-field block fixture (mutate the honest compact's positional
  commitment to `computeHash32(badPreimage)`, re-derive
  `witness_set_hash`/tx id as the mutation demands, re-encode, commit
  `(id -> compactCbor)` into the single-leaf transactions MPF), removal
  category record, reference-script publication, and the raw step builders.
  On-chain refusal honesty guard imported from the decoding support module.

### 8.4 Registration completion and remaining operations

The production id, category-order append, generic `submitInit`, manifest
entries, and watcher proof-thread topology are complete. Family-specific CLI,
watcher detection/prover mounting, and published-chunk/tier-3 carriage exercise
remain open; topology registration is not autonomous actuation.

## 9. Sequencing and dependencies

1. Blueprint build + family Aiken tests green (done: 88/88).
2. `src/canonical-decodability/` modules (§4.2).
3. Test-support extensions (§8.3).
4. Suites 1–3 (§8.2), run green.
5. `catalogue-status.md` §1 row 19 update (this family's row only).
6. Registration/deployment wiring (§8.4) — complete; operational adapters open.

Dependencies: none on the sibling committed-field-shape family (§12.8) —
its wave runs separately and shares only the pre-existing SDK/door
machinery. Nothing here touches `onchain/`.

## 10. Decision register

- **D1 — Category id `00000011`, canonical.** It is routed through the
  production catalogue and deployment manifest.
- **D2 — Reference scripts, unconditionally.** Owner ruling 2026-08-26:
  fault-proof scripts deploy as reference scripts regardless of size; the
  suites require them on the removal path (`requireReferenceScripts: true`)
  and drive every step spend through the published UTxOs.
- **D3 — No new binding path.** Transaction binding is exclusively the
  shared `pass_native_tx_to_next_step_carried`; the family inherits both
  inclusion carriages from it.
- **D4 — Submitter accepts both inclusion carriages.** The published-chunk
  transport is the same shared type and machinery already emulator-proven by
  the decoding family; this family pins both wire encodings without duplicating
  the transport's full journey under a second id.
- **D5 — Submitter implements `Inline` field carriage.** Tier 2/3 field
  carriage is pinned on-chain by the family's own Aiken selectors (tier 3 at
  15,200 bytes) and the publication helper exists family-agnostically;
  emulator exercise of tier 3 under this family remains operational test work.
  The pure prepare/submit path therefore refuses preimages above the 14,336-byte
  inline frontier with an instruction to publish a `RawUtxo`/`Certified`
  carriage; it never silently mislabels a larger preimage as `Inline`.
- **D6 — Thin family cancel submitter included.** `ct.Cancel` remains the
  shared arm, while `submit-canonical-decodability-cancel.ts` adds only
  family-step location, mandatory reference-script verification, prover
  authentication, and the shared cancellation burn. Emulator coverage pins
  cancellation from both step addresses.
- **D7 — Detection ships as the pure prepare core.** Watcher wiring is
  parent-owned (§3, §8.4); the prepare module is the fail-closed nucleus it
  will call.

## 11. Out of scope

Family-specific CLI/watcher actuation and the remaining §8.4 operations; the §12.8 sibling family;
DA-layer preimage-availability faults (a committed hash whose preimage the
DA layer cannot produce is a different fault); any change under
`onchain/aiken/`; preprod/network acceptance.
