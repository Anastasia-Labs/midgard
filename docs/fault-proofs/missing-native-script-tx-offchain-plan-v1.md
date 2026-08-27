# Missing-native-script-tx fault: offchain implementation plan (v1)

> **Registration update (2026-08-26):** this family is now registered as
> `missingNativeScriptTx` at `0000000f`. Generic Init,
> catalogue/inspection, node/core deployment identity, watcher proof-thread
> topology, and all six mandatory authenticated reference scripts are wired.
> Family-specific CLI, autonomous watcher detector/prover mounting, preprod,
> and live evidence remain open. The identity change requires fresh
> genesis/redeployment; there is no migration or compatibility path.

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`),
which already carries the missing-signature plan (Q16) and the
native-script-decoding removal-leg extension this family builds on. Task:
**Q17** (`GOAL_SPEC.md` §9.3). Catalogue row: `catalogue-status.md` §1
row 9. Unlike the Q16 document, this plan ships **with its
implementation**: the family modules, harness extensions, and emulator
suites it describes land in the same change set.

Implementation status (2026-08-26): **complete and registered**. The strict SDK
twins, explicit contracts/evidence/prepare/init/six-step/cancel modules,
catalogue harness wiring, two-transaction fixture, uniform
reference-script path, absent/present emulator polarities, earlier adversarial
controls, permanent-token mint, explicit-category removal, and cancellation
are implemented and verified. Family-specific CLI, watcher detector/prover
mounting, and proving-core orchestration remain intentionally open.

The parity bar is `missing-signature-offchain-plan-v1.md`, which in turn
inherits the `native-script-decoding` family's generic decisions.
Everything decided there is inherited here, not re-decided:

- **Reference scripts always (owner ruling 2026-08-26):** all six step
  validators deploy as reference scripts and are referenced, never
  attached inline, regardless of compiled size (§2.3).
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through the full lifecycle — fraud-proof mint **and**
  fraudulent-commitment removal — and an adversarial prover against an
  honest commitment is refused **on-chain at the exact check** (§8).
- **Explicit-record discipline:** contracts remain explicit while
  `missingNativeScriptTx` is canonically routed through catalogue and
  deployment manifests. Family-specific CLI remains separate.
- **Canonical id:** `0000000f` is the production category id.
- **Removal via explicit category:**
  `RemoveFraudulentBlockExplicitCategory` drives removal with every
  fail-closed check intact and zero changes to
  `remove-fraudulent-block.ts`. The fraud-proof token is permanent by
  design; the state-queue node NFT burns.
- **Cancellation is an explicit prover decision.**

---

## 1. The contract the builders must satisfy

The as-built onchain family
(`onchain/aiken/validators/fraud-proofs/missing-native-script-tx/step-0{1..6}.ak`,
lib wire twins `lib/midgard/fraud-proofs/missing-native-script-tx/`) is
the byte-for-byte target. The fault statement: a transaction the operator
committed spends an output locked by a **script credential** whose script
is a native Cardano script (canonical versioned-script hash, language tag
0), and that script is **absent** from the transaction's script-witness
collection (witness field 6). Decisive checks: `step-05.ak:73-79`
(native-hash classification) and `step-06.ak:127-150` (the absence fold).
Single direction — wrongful acceptance only.

**Step chain** `Init → 01 → 02 → 03 → 04 → 05 → 06`, `ct.Cancel` on every
step; eight L1 transactions, no self-loop, no scan-plan analogue.
Parameterization (acyclic, applied backwards, step-06 first):

| Validator   | Parameters (blueprint-declared order)                                                                                                    | Unapplied size |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------------- | -------------- |
| `…/step_01` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`                                                      | 5,775 B        |
| `…/step_02` | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id`                            | 7,117 B        |
| `…/step_03` | `step_04_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`                                                      | 5,846 B        |
| `…/step_04` | `step_05_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id`                            | 8,736 B        |
| `…/step_05` | `step_06_validator_script_hash`, `computation_thread_token_policy_id`                                                                    | 1,570 B        |
| `…/step_06` | `computation_thread_token_policy_id`, `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `field_preimage_certificate_policy_id` | 7,642 B        |

Two binding steps (01 and 03 — the only family so far with **two**
`pass_native_tx_to_next_step` invocations in one thread), three
certificate-parameterized steps (02, 04, 06 — three distinct §8.8 door
openings), and a step-06 that leads with the computation-thread policy
(unlike missing-signature's step-04, which leads with the fraud-proof
pair). The contracts record's docblock pins this order.

**Step-01** (`step-01.ak`): `Continue(NativeTxInclusionArgs)` — the shared
native binding of the **bad** transaction (counted `transactions_root`
authentication + PHAS membership over the raw compact CBOR). **Bare args
only** — no published-chunk carriage arm on-chain. Output:
`step_02.State { bad_tx_id, bad_tx_witness_set_hash }`. The second field
is the family's load-bearing move: §3's transaction id commits the _body_
alone, so step-01 reads the real `witness_set_hash` off the
block-committed compact structure and it rides the thread as the §2.5
anchor all the way to step-06. No redeemer downstream can restate it.

**Step-02** (`step-02.ak`): opens body field 0 (`spend_inputs`) of the bad
transaction through the §8.8 door (`BodyAnchor { tx_id: bad_tx_id }`) and
selects the accused input by `bad_input_index` via the fixed 40-byte
stride (`spend_input_at`; out-of-domain ordinal aborts). Args:
`{ input_index, output_index, bad_input_index, spend_inputs_opening }`.
Output: `step_03.State { input_with_missing_script: MidgardTxInput,
bad_tx_id, bad_tx_witness_set_hash }`.

**Step-03** (`step-03.ak`): the **second** native binding — this time of
the _producing_ transaction, the one that created the accused input's
UTxO. `Continue(NativeTxInclusionArgs)` again, plus the family check
`producing_tx_id == input_with_missing_script.tx_id` (`step-03.ak:64`).
Output: `step_04.State { producing_tx_id, bad_input_output_index,
bad_tx_id, bad_tx_witness_set_hash }` where `bad_input_output_index` is
the output index the accused input names.

**Step-04** (`step-04.ak`): opens body field 2 (`outputs`) of the
producing transaction (`BodyAnchor { tx_id: producing_tx_id }`), reads the
output at `bad_input_output_index` (`field_item_at` — variable-width, a
§5.1 head walk per item), decodes it (`decode_midgard_tx_output_cbor`)
and requires a **script** payment credential:
`expect ScriptCredential(expected_missing_script_hash) = …` — a
key-locked output refuses right here (`step-04.ak:91`). Output:
`step_05.State { expected_missing_script_hash, bad_tx_id,
bad_tx_witness_set_hash }`.

**Step-05** (`step-05.ak`): the native-script classification lift — the
prover supplies `missing_native_script_bytes` and the step checks
`expected_missing_script_hash == versioned_script_hash(
MidgardVersionedScript { language: NativeCardanoScript, script_bytes })`
(blake2b-224 over `0x00 ‖ script_bytes`, `script-proof-v1.ak`). Cheapest
step (1,570 B); no field opening, no reference inputs. Output:
`step_06.State` (same three fields). The need for the script _preimage_
here creates the family's completeness corner (§7.2), the exact analogue
of missing-signature's vkey corner.

**Step-06** (`step-06.ak`): `common.finalize` burns the thread NFT and
mints the permanent fraud-proof token. Opens witness field 6
(`script_tx_wits`) through the door with `WitnessAnchor { tx_id:
bad_tx_id, witness_set_hash: bad_tx_witness_set_hash }` — the door
re-derives the supplied witness set against the **thread-anchored** hash,
which is the entire soundness of the step (`step-06.ak:86-121`): a door
checking only the tx id would accept an empty witness set against any
transaction, making "the script is absent" true of everything ever
committed. Then one fold walks the authenticated preimage; each item is
prefix-decoded (`decode_midgard_versioned_script_at`) and **re-encoded**
(`expect encode_midgard_versioned_script(script_wit) == item`, §6.1
canonicality — trailing junk or a non-minimal length prefix refuses), and
the accumulated `versioned_script_hash(script_wit) ==
expected_missing_script_hash` must end `False` (`step-06.ak:127-150`).
Args: `{ input_index, output_index, fraud_proof_mint_redeemer_index,
script_tx_wits_opening }`.

**Carriage limit (§8.3 erratum E2 limit 2).** Field 6 is variable-width,
so the fold needs `field_item_count`, which is authenticated only for
tiers 1–2 — under tier-3 Certified carriage the count is the §5.1 header's
self-assertion and `field_item_count` aborts. A prover whose field-6
preimage does not fit tier-2 carriage cannot finalize this family at all;
the abort is loud and recorded, and lifting it is #565/#579's job. The
offchain planner (`planMidgardFieldCarriageV1`) decides the tier from the
preimage length and never lets the caller choose, so the builder cannot
route into the aborting tier silently.

**The onchain selector inventory is the adversarial-suite spec.** The
validator files carry the exact forgeries the emulator suites reproduce
against a live commitment: absent accepts / present refuses
(`step-06.ak:235-255`), truncated preimage (`:261`), one-sided forged
witness set (`:276`), the **both-sides forgery** — genuine body bytes with
an empty-witness-set tail, refused only by the thread-anchored hash
(`:310`), the substituted witness set that drops just the accused script
(`:329`), the §6.1 non-canonical item (`:358`), the key-locked-output
refusal at step-04 (`step-04.ak:175`), and substituted-preimage refusals
at steps 02 and 04.

## 2. Registration

### 2.1 Category id

Canonical category id: **`0000000f`** (index 15). The SDK catalogue,
generic Init, deployment manifests/inspection, and watcher proof-thread
topology bind it to the applied step-01 hash.

### 2.2 Registered deployment surface

The SDK catalogue order, `FraudProofs`/`FaultProofContracts` records,
deployment-manifest identity, node manifest/descriptors/catalogue-MPF build,
generic CLI category parse, inspection, and watcher proof-thread topology are
wired. Family-specific CLI and autonomous watcher detector/prover mounting
remain open. Adoption is a fresh genesis-level deployment only.

### 2.3 Script deployment: reference scripts

All six steps deploy as reference scripts
(`publishPlainReferenceScriptUtxo`, oversized shape for uniformity) and
every production submitter takes a `referenceScriptUtxo` sourced through
the fail-closed `requireMissingNativeScriptTxReferenceScriptV1` hash
check. Inline attachment is not a production fallback. The emulator suites
drive the reference-script path exclusively.

## 3. Detection

Detection follows the missing-signature plan §3 shape and is **not mounted
autonomously**. The watcher has a proof-thread topology entry, but the
recognition predicate an offchain scanner needs is already expressible
with what this wave ships: for each committed tx, for each spend input,
resolve the producing output; if its payment credential is a script hash
that equals `hashMidgardVersionedScript` of a known-preimage **native**
script and no item of the tx's field-6 preimage hashes to it, the fault
is provable. The SDK rule helpers (§4.1) are exactly this predicate,
factored so a future finding-record/prover-core wave reuses them
unchanged. The unknown-preimage corner is classified out before proving
(§7.2).

## 4. New offchain modules

### 4.1 SDK (`demo/midgard-sdk/src/fraud-proof/missing-native-script-tx-v1.ts`)

The strict TypeScript twin of the six lib records — field order mirrors
the aiken declarations 1:1. Ships:

- Step 01–06 `State`/`Datum`/`Args`/`SpendRedeemer` schemas
  (`faultProofStepDatumSchema` / `faultProofStepRedeemerSchema` wrappers;
  steps 01/03 alias `NativeTxInclusionArgsSchema` — bare, **not** the
  carriage enum).
- `missingNativeScriptTxVersionedScriptHashV1(scriptBytes)`: the
  `versioned_script_hash` twin for language tag 0, delegating to core's
  `hashMidgardVersionedScript` (blake2b-224 over `0x00 ‖ bytes`; core
  also canonicality-checks the native-script encoding).
- `missingNativeScriptIsAbsentV1({ scriptTxWitsItems,
expectedMissingScriptHash })`: the step-06 fold's offchain twin —
  prefix-decode + re-encode canonicality + hash accumulation — used by
  submitters as the pay-before-prove local pre-run and by tests as the
  polarity oracle.
- State builders (`missingNativeScriptTxStep02StateFromBadTxV1`, etc.)
  mirroring each step's forwarding rule.

Exported from the SDK fraud-proof index; no catalogue surface touched.

### 4.2 Family modules (`demo/midgard-fault-proofs/src/missing-native-script-tx/`)

Mirrors `src/native-script-decoding/` (the explicit-contracts,
family-module pattern):

- `contracts-v1.ts` — blueprint titles, `StepContractV1` six-tuple record
  (+ shared computation-thread / fraud-proof / hub-oracle / state-queue /
  certificate ids), **no categoryId field**, parameter-order docblock.
- `submit-common-v1.ts` — category record type, thread-UTxO /
  reference-script / step-state fail-closed requires.
- `submit-missing-native-script-tx-init.ts` — catalogue-membership PHAS
  withdrawal + thread NFT mint (asset name = categoryId ‖ headerHash) +
  step-01 payout, exactly the decoding init's shape.
- `submit-missing-native-script-tx-step-01.ts` / `-step-03.ts` — the two
  native bindings: hub-oracle + state-queue reference inputs, header-hash
  match against the thread token, `Continue(bare NativeTxInclusionArgs)`
  with builder-resolved indices, PHAS membership withdrawal over the raw
  compact CBOR. Step-03 additionally pre-checks `producingTxId ==
input_with_missing_script.tx_id` from the thread state.
- `submit-missing-native-script-tx-step-02.ts` / `-step-04.ts` — §8.8
  door steps: `planFaultProofFieldOpeningV1` (fields 0 / 2, BodyAnchor),
  `publishFaultProofFieldCarriageV1` when the planner escalates tiers,
  carriage-aware fee-input selection, positional redeemer resolution.
  Step-04 pre-extracts the script credential offchain and refuses
  key-locked outputs before paying.
- `submit-missing-native-script-tx-step-05.ts` — plain continue carrying
  `missing_native_script_bytes`, with the local versioned-hash pre-check.
- `submit-missing-native-script-tx-step-06.ts` — finalize: field-6
  WitnessAnchor opening (`anchorWitnessSetHash` read from **thread
  state**, never re-derived from prover input), local absence pre-run,
  thread-burn + fraud-proof mint + `FraudProofTokenDatum` payout, exactly
  the invalid-signature finalize machinery.
- `submit-missing-native-script-tx-cancel.ts` — explicit prover-decision
  cancel for any step.
- `index.ts` barrel, re-exported from `src/index.ts`. **No `bin.ts`
  verbs.**

### 4.3 Proving core

Deferred to the registration/watcher wave, as for missing-signature
(§4.3 there). The per-step submitters are consumer-agnostic and carry all
fail-closed checks, so the future core is an orchestration layer only.

## 5. Carriage frontiers

Inherited from the decoding/missing-signature analysis; the family adds
no new frontier class. Step-01/03 membership proofs ride the redeemer
(bare args; the on-chain family has no published-chunk arm, so a proof
that outgrows the envelope is a _binding_ blocker shared with
missing-signature — recorded, not solved here). Fields 0 and 2 openings
are body-tier plans; field 6 is witness-tier with the E2-limit-2 tier-3
abort (§1). The emulator fixtures keep every preimage in tier 1 (inline).

## 6. Economics and pacing

Eight transactions per proof (init + six steps + removal ride-along).
Same pacing/budget posture as missing-signature §6; nothing
family-specific beyond the extra binding step's PHAS withdrawal cost.

## 7. Cancel, recovery, and the corners

### 7.1 Crash-resume

Thread state is entirely on-chain; every submitter re-derives its inputs
from the thread UTxO datum plus the block fixture, so re-running the same
step after a crash either continues or fails closed on the already-spent
out-ref. Nothing to persist offchain.

### 7.2 The unknown-preimage corner (recorded; refused at classification)

Step-05 needs the accused script's **bytes**. A script-locked output whose
hash has no known preimage (the L2 owner locked to a hash never revealed)
is a real fault this family cannot _finalize_. Same disposition as
missing-signature §7.2: the scanner classifies such findings as
not-provable-here rather than opening a thread that stalls at step-05.
The hash-mismatch face — prover supplies bytes whose versioned hash is
not the credential — refuses at `step-05.ak:73-79` and is emulator-tested.

### 7.3 Present-but-hash-mismatched routing (recorded, not open)

A witness collection that _contains_ a script whose bytes hash to a
**different** credential than the accused one does not excuse the fault:
the fold matches by canonical versioned hash, not by presence of "some"
script. Conversely, a transaction carrying the correct script is honest —
the fold finds it and finalize refuses. Both polarities are in the
adversarial suite. Plutus-locked (tag 3/128) outputs are a different
family's subject: step-05's tag-0 hash equation refuses a prover trying
to shoehorn one here, because the credential hash is prefixed with the
actual language tag at output creation.

**Timelock-model scripts.** `after`/`before` and composite native
scripts are hashed like any other native script — the family's rule is
presence, not satisfiability, so a timelock script that was absent is
provable identically. The emulator fixture set includes a composite
(`all [sig]`) script; the codec twins (`encodeMidgardNativeScript`)
already cover the timelock constructors, and the classification lift is
byte-level, so no separate onchain path exists to test.

## 8. Testing

### 8.1 Where and how

Vitest + lucid-evolution emulator, one heavy journey per **file**
(`@lucid-evolution/uplc` wasm heap is never reclaimed; vitest isolates
per file — `tests/support/uplc-heap-guard.ts`). Suites use the real
regenerated blueprint via the pinned aiken fork. Refusals assert through
`expectOnchainRefusalV1` (`/failed script execution/`), with raw
builders that bypass the submitters' own fail-closed guards so the
refusal observed is the **validator's**.

### 8.2 Suites, in order of construction

1. `tests/submit-init-emulator-missing-native-script-tx-lifecycle.test.ts`
   — real-fault polarity: committed block whose bad tx spends the
   script-locked output of an also-committed producing tx with an empty
   script-witness collection; init → steps 01–06 driven through the
   submitters over published reference scripts; thread NFT burned at
   every step address; permanent fraud-proof token +
   `FraudProofTokenDatum{fraud_prover}`; then the removal leg —
   `submitRemoveFraudulentBlock` with the explicit category record:
   state-queue node NFT burned, root relinked `Empty`, operator slashed,
   scheduler rewound, fraud-proof token surviving untouched, second
   removal refused. Also: cancel polarity on a second thread (Cancel at
   step-02 refunds the prover and burns the thread NFT).
2. `tests/submit-init-emulator-missing-native-script-tx-adversarial.test.ts`
   — honest-commitment polarity: a block whose transaction **carries**
   the script. The full six-step thread drives to step-06 and the
   finalize is refused at the exact `required_script_is_present == False`
   check (raw builder; the honest submitters would refuse locally).
   Negative controls at earlier steps against the real-fault block:
   step-04 raw continue against a key-locked output; step-05 raw continue
   with mismatched script bytes; step-03 binding a non-producing tx;
   step-06 with a truncated field-6 preimage (door refusal). Outsider
   control: a third wallet cannot cancel the prover's thread.

Corner-cases in §7.2/§7.3 that refuse _offchain_ (unknown preimage,
submitter pre-checks) are asserted as thrown submitter errors, not
emulator refusals.

### 8.3 Test-support extensions (extend, do not fork)

- `tests/support/emulator/native-tx.ts`: optional
  `scriptTxWitsPreimageCbor` on `makeNativeTx` (defaults preserved).
- `tests/support/emulator/contracts.ts`:
  `realMissingNativeScriptTx` option + `buildMissingNativeScriptTxChainV1`
  (backwards application through `applyCompiledScript`, step-06 first) +
  explicit contracts record + return spread.
- `tests/support/emulator/harness.ts`:
  `MISSING_NATIVE_SCRIPT_TX_TEST_CATEGORY_ID_V1 = "0000000f"` + the
  extra-categories spread registering step-01's hash under the test id.
- `tests/support/emulator/removal-deployment.ts`:
  `MISSING_NATIVE_SCRIPT_TX_REMOVAL_DEPLOYMENT_ENTRY_V1 =
"fraudProofMissingNativeScriptTx"` + manifest spread.
- New `tests/support/missing-native-script-tx-emulator-v1.ts`: harness
  wrapper, two-tx trie fixture builder (producing tx with the
  script-locked output at the accused index; bad tx spending it; both
  MPF leaves of one committed block), reference-script publication,
  explicit removal-category record, raw adversarial builders.

### 8.4 Registration completion and remaining operations

Catalogue order, generic Init, deployment-manifest identity, reference scripts,
and watcher proof-thread topology are complete. Family-specific CLI, proving
core orchestration, and autonomous watcher actuation remain open (§2.2, §4.3).

## 9. Sequencing and dependencies

This wave is self-contained given HEAD `a1724e63`: the on-chain family is
complete and blueprint-verified with the pinned fork
(`/home/gumbo/.aiken-fork/bin/aiken`, v1.1.23+2a78108); the explicit-
category removal seam is landed; the §8.8 offchain door
(`src/field-opening-v1.ts`) and the finalize machinery
(invalid-signature step-02) are the reused templates. No onchain change.
The parallel Q16 wave shares harness insertion points; both add disjoint
keys/ids, so the merge is textual.

## 10. Decision register

- **D1 — Category id `0000000f`, canonical production registration.**
- **D2 — Reference scripts always** (owner ruling 2026-08-26).
- **D3 — Bare `NativeTxInclusionArgs` at steps 01/03** — the on-chain
  `Args` alias is the bare record, not `NativeTxInclusionCarriage`;
  emitting the enum would be a positional mis-encode.
- **D4 — Init in `src/`, not the harness fabricated-init** — the
  fabricated-init helper is typed to the fabricated-family union; the
  decoding family's own init submitter is the pattern for explicit
  categories and is mirrored, not widened.
- **D5 — Anchor hash from thread state only.** Step-06's
  `anchorWitnessSetHash` is read from the step-06 datum, never
  recomputed from prover-supplied witness data — the offchain twin of
  the both-sides-forgery refusal.
- **D6 — Fixture output credential = versioned hash.** The producing
  output's address is `0x70 ‖ versioned_script_hash(tag 0, bytes)` —
  matching step-05's equation; a plain (untagged) script hash would make
  the honest fixture unprovable, which is itself asserted in tests.
- **D7 — Unknown-preimage findings refuse at classification** (§7.2),
  mirroring the Q16 disposition; no on-chain escape hatch is added.
- **D8 — One emulator journey per file** (wasm heap isolation).

## 11. Out of scope

Family-specific CLI, the proving core and autonomous watcher integration
(§4.3), published-chunk carriage for the two binding proofs (no on-chain
arm), the #565/#579 tier-3 field-6 lift, and any onchain change.
