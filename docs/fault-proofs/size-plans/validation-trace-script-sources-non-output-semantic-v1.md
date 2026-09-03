# Size-fit plan: `script_sources_non_output_semantic_v1`

Cites [00-primer.md](00-primer.md). This plan also defines the **shared raw
stage-frame library** (§4.1) and the **shared semantic-yield handshake**
(§4.2) that every other script-sources plan in this directory references.

## 1. Identity

| Field                  | Value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint title        | `fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                                |
| File                   | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-non-output-semantic-v1.ak` (87 lines)                                                                                                                                                                                                                                                                                                                                                                                                    |
| Raw size               | **115,590 bytes** (largest script in the catalogue; limit 16,384, target 15,000)                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Applied parameters     | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`, `field_preimage_certificate_policy_id: PolicyId`                                                                                                                                                                                                                                                                                                                                                                                     |
| Phase / index          | `ScriptSources` (resolver index 8), semantic slot **0** of 29 (`script_sources_semantic_resolver_count = 29` in `lib/midgard/validation-resolver-v1.ak`), global index 32 (`validationSemanticResolverGlobalIndexV1(8, 0)`)                                                                                                                                                                                                                                                                                     |
| Library entry point    | `validation_machine_v1.verify_script_sources_non_output_semantics_v1(pre, ValidationOneStepEvidenceV1 { transition, auxiliary }, door)` — a 12-arm `if control.stage == k` dispatch over `script_sources_stage_{zero,one,two,three,four,six,seven,eight,nine,ten,eleven,twelve}` (stage 5 excluded)                                                                                                                                                                                                             |
| Redeemer               | `ct.StepRedeemer<ActionV1>`, `VerifyNonOutput { input_index, output_index, transition: ValidationOneStepWitnessV1, auxiliary: ValidationAuxiliaryWitnessV1 }`                                                                                                                                                                                                                                                                                                                                                   |
| Role name today        | none (no `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` entry; published as a plain script-ref UTxO)                                                                                                                                                                                                                                                                                                                                                                                                                       |
| Deployment entry today | none. Wired only in `demo/midgard-sdk/src/fraud-proof/contracts.ts` (`VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.scriptSourcesNonOutput`). `demo/midgard-fault-proofs/src/validation-dispute/submit.ts` builds its redeemer (`semanticActionFieldsV1`, resolver 8, `semanticResolverIndex === 0`) but has no named deployment entry or `require…ReferenceScriptUtxo` helper. The emulator publishes it with `oversized: true` (`tests/support/emulator/dispute-scenario.ts`, `semanticIsOversized`). |

Stages served **only** by this monolith today: **2, 3, 4, 6**. Every other
non-output stage already has narrow resolvers (stage 0: slots 5–9; stage 1:
slots 14, 15, 28; stage 7: 25–27; stage 8: 23–24; stage 9: 10–13; stage 10:
20–22; stage 11: 16–17; stage 12: 18–19).

## 2. Why it is this size

Probe copy `/tmp/size-probe-ssa` (pinned fork `v1.1.23-org-5adf7837`, `--env
testnet`), procedure per primer. Every probe below keeps the deployed
validator shell (`cancel` + `continue_winning` + typed
`ValidationOneStepWitnessV1`/`Datum` decode) and adds one sub-function; the
delta column is the cost of the added code. Private helpers were made `pub`
in the copy only.

| Probe                 | Adds                                                                                                                                                                             |                         Raw bytes |                                 Delta |
| --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------: | ------------------------------------: |
| p00 shell             | nothing (`semantic_transition_is_valid = True`)                                                                                                                                  |                             3,349 |                                     — |
| p01                   | `expect aux: ValidationAuxiliaryWitnessV1 = data` (40-constructor decoder)                                                                                                       |                            16,702 |                           **+13,353** |
| p02                   | `structural_transition_is_valid`                                                                                                                                                 |                             4,762 |                                +1,413 |
| p45                   | `script_sources_control_from_data_items` (30 base items, no extension)                                                                                                           |                             5,373 |                                +2,024 |
| p44                   | p45 + `encode_script_sources_witness` (base encoder)                                                                                                                             |                             8,001 |                                +4,652 |
| p03                   | `script_sources_control_from_witness` (adds the stage-5/8+ extension decoders: `ledger_output_proof_v1.decode_control_v1`, `script_discovery_control_from_cbor`)                 |                            25,595 |                           **+22,246** |
| p56                   | p45 + `script_sources_control_successor_is_exact` (→ `exact_script_sources_control`: four encoders incl. `encode_script_sources_output_proof_witness` and the discovery encoder) |                            24,944 |                  **+19,571 over p45** |
| p04                   | p03 + `script_sources_control_is_bound`                                                                                                                                          |                            35,851 |                      +10,256 over p03 |
| p06 / p06b            | `verify_native_tx_proof_source_v1` / decode-only                                                                                                                                 |                     5,351 / 4,768 |                       +2,002 / +1,419 |
| p07                   | p06 + `field_door.open_machine_field_item`                                                                                                                                       |                             8,514 |                                +3,163 |
| p08                   | p07 + `native_script_scan_v1.versioned_script_header_v1`                                                                                                                         |                             9,614 |                                +1,100 |
| p27 stage two         | p03 + aux decoder + `script_sources_stage_two`                                                                                                                                   |                            45,744 | +2,456 over the 43,288 dispatch base¹ |
| p28 stage three       | … `script_sources_stage_three` (finish + `script_sources_replay_item`)                                                                                                           |                            46,152 |                                +2,864 |
| p29 stage four        | … `script_sources_stage_four` (door)                                                                                                                                             |                            52,768 |                                +9,480 |
| p30 stage six         | … `script_sources_stage_six` (door + mint policy/asset fold)                                                                                                                     |                            57,540 |                               +14,252 |
| p26 stage zero        | … `script_sources_stage_zero`                                                                                                                                                    |                            61,394 |                               +18,106 |
| p25 stage one         | … `script_sources_stage_one` (`redeemer_item_proof_v1.step_v1`)                                                                                                                  |                            83,150 |                               +39,862 |
| p31 / p32 / p33 / p35 | stage seven / eight / nine / eleven                                                                                                                                              | 54,774 / 42,739 / 44,425 / 43,355 |            +11,486 / ≈0 / +1,137 / ≈0 |
| p34 / p36             | stage ten / twelve (redeemer-item traversal)                                                                                                                                     |                   73,653 / 73,375 |                     +30,365 / +30,087 |
| p53                   | p45 + `script_sources_replay_item` (uses the base encoder)                                                                                                                       |                            11,948 |                                +8,599 |
| p54                   | p45 + native source + door + `script_sources_begin_mint_policy` (uses `exact_script_sources_control`)                                                                            |                            33,561 |                                see §5 |
| p55                   | p45 + `script_sources_fold_mint_asset` (uses `exact_script_sources_control`)                                                                                                     |                            29,900 |                                see §5 |
| p39                   | raw 30-item frame + canonical-head check + stage-byte splice + `hash_work_witness` successor (no typed control)                                                                  |                             5,741 |                                +2,392 |
| p42                   | raw 31-item frame + extension splice + `state_queue_yield.require_authenticated_zero_yield`                                                                                      |                             6,326 |                                +2,977 |
| y00                   | withdraw-purpose yield shell: locate dispatcher input, read datum + spend redeemer                                                                                               |                             1,663 |                                     — |

¹ dispatch base = p03 (25,595) + aux decoder (13,353) + `exact_script_sources_control` reach (≈4,340) = 43,288.

Dominators, in order: (1) the twelve stage bodies (stages 1, 10, 12 alone
add ~100 KB because each reaches `redeemer_item_proof_v1.step_v1` →
`cek_data_traverse_v1.step_v1`, measured 40,086 / 32,874 bytes in p24/p24b of
the output-proof-step plan); (2) `exact_script_sources_control` +
`script_sources_control_from_witness` (~20 KB each, because the stage-5 and
stage-8+ extension codecs — `ledger_output_proof_v1` control codec measured
18,452 bytes — are reachable from the generic parser/encoder regardless of
stage); (3) the generic `ValidationAuxiliaryWitnessV1` decoder (13,353 bytes)
pulled in by the `auxiliary: ValidationAuxiliaryWitnessV1` redeemer field.

## 3. Options considered

- **Prune (pattern 1) alone — rejected as sufficient.** Removing the eight
  arms that already have narrow resolvers leaves stages 2, 3, 4, 6 behind the
  generic parser, encoder and auxiliary decoder: ≥43 KB before any arm.
  Pruning is still applied inside every new piece (raw frame instead of
  typed control, raw constructor reads instead of the 13 KB decoder).
- **Withdraw-zero yield split (pattern 2) — chosen.** Keep slot 0 as a
  ≤7 KB dispatcher for exactly the four stages without narrow resolvers and
  move each arm into a role-authenticated yield. Keeps
  `script_sources_semantic_resolver_count = 29`, the prepare parameter list
  cardinality, `VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1[8]`, the offset table
  and the 91-entry global index unchanged.
- **New prepare-selected resolvers per arm (redesign of the group) —
  rejected.** Eight extra slots would move `script_sources_semantic_resolver_count`
  to 37, shift `VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1` for indices 9–13,
  break the `28 → 90` special case in `validationSemanticResolverGlobalIndexV1`,
  and re-key every inspection fixture (`semantic-resolver-${index}` ×91).
  Functionally equivalent to the yield split with strictly more ripple.
- **Multi-transaction chaining (pattern 3) — not needed.** Every arm fits a
  single transaction; the only chaining in this family is the stage-one
  redeemer route (its own plan).

## 4. Chosen design

### 4.1 Shared: raw stage-frame library (new `lib/midgard/script-sources-raw-frame-v1.ak`)

Used by every plan in this group. It replaces `script_sources_control_from_witness`
/ `script_sources_control_is_bound` / `exact_script_sources_control` in the
narrow resolvers with byte-level operations on the canonical work witness.

```aiken
pub type RawScriptSourcesFrameV1 {
  items: List<Data>,              // 30 or 31 un_list_data items
  item_count: Int,
  prefix: ByteArray,              // canonical head: header ++ items 0..8 re-encoded
  stage_offset: Int,              // byte offset of the stage item
  compact_cbor: ByteArray, witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray, context_cbor: ByteArray,
}
pub fn open_frame_v1(pre, witness, expected_item_count, expected_stage) -> RawScriptSourcesFrameV1
pub fn item_int_v1(frame, index) -> Int          // builtin.un_i_data
pub fn item_bytes_v1(frame, index) -> ByteArray  // builtin.un_b_data
pub fn item_frontier_v1(frame, index) -> List<FrontierPeak>
pub fn replace_stage_v1(frame, witness_cbor, new_stage) -> ByteArray
pub fn splice_v1(witness_cbor, offset, old_encoding, new_encoding) -> ByteArray
pub fn append_extension_v1(witness_cbor, extension) -> ByteArray       // 30 -> 31 items
pub fn replace_extension_v1(witness_cbor, old_ext, new_ext) -> ByteArray
pub fn drop_extension_v1(witness_cbor, old_ext) -> ByteArray           // 31 -> 30 items
pub fn successor_is_exact_v1(pre, witness, next_cbor) -> Bool
pub const empty_observer_scan_cbor: ByteArray   // == encode_observer_purpose_scan_control(empty_observer_purpose_scan_control())
pub const empty_mint_fold_cbor: ByteArray       // == encode_mint_fold_control(empty_mint_fold_control())
pub const initial_output_scan_cbor: ByteArray   // == ledger_output_scan_v1.encode_control_v1(initial_control_v1())
```

`open_frame_v1` checks (measured 2,392 bytes over the shell, probe p39):
`cbor.deserialise` + `un_list_data`; `list.length(items) == expected_item_count`;
the canonical head `encode_definite_array_header(n) ++ encode_definite_bytes(items 0..3)
++ cbor.serialise(resolved_input_count) ++ encode_definite_bytes(accumulator)
++ cbor.serialise(signer_count) ++ encode_definite_bytes(signer_frontier)
++ validation_merkle_v1.encode_frontier(resolved_item_peaks)` equals
`slice(0, len)` of the witness; `native_tx_proof_commitment_v1(compact, ws,
lengths) == pre.transaction_commitment`; `hash_validation_context(context) ==
pre.validation_context_hash`; `un_i_data(items[9]) == expected_stage`.
`splice_v1` requires `slice(offset, len(old)) == old` before replacing, so a
redeemer-supplied offset that does not land on the claimed item fails closed.
The three literals are pinned by golden tests against the encoders (§8).

**Soundness of splicing without re-encoding (canonical-by-induction).**
`prepare_selected` runs `validation_resolution_v1.prepare_semantic_resolution`,
which requires `structural_transition_is_valid(pre_state, transition)` —
i.e. `hash_work_witness(pre.phase, pre.program_counter, work_witness_cbor) ==
pre.work_root` — and freezes `evidence_hash = hash_one_step_evidence(transition,
auxiliary)`; `continue_winning` re-checks that hash. So the bytes a narrow
resolver splices are exactly the bytes committed at `pre.work_root`. Those
bytes were produced by the previous accepted step's successor construction
(`exact_script_sources_control` / `encode_script_sources_witness` today, a
canonical splice of a canonical predecessor after this plan), and the first
ScriptSources control is produced by the ResolveInputs finish step with
`encode_script_sources_witness`. Hence every item the splice copies verbatim
is canonical, and every item it rewrites is re-encoded with the same
canonical encoder the monolith used. This is the argument the RF-021 envelope
binder already relies on (`verify_raw_envelope_v1`, comments at
`script-sources-redeemer-normalization-v1.ak:372-397`) and the stage-zero
narrow resolvers already use (`script_sources_stage_zero_begin_successor_is_exact`).

### 4.2 Shared: semantic-yield handshake (new `lib/midgard/validation-semantic-yield-v1.ak`)

```aiken
pub fn require_semantic_yield_v1(tx, reference_script_auth_policy_id, role: AssetName, yield_ref_input_index: Int) -> ScriptHash
  // = state_queue_yield.require_authenticated_zero_yield(tx.reference_inputs, tx.withdrawals, tx.redeemers, policy, role, index)
pub fn unique_semantic_dispatch_v1(dispatcher_script_hashes: List<ScriptHash>, tx) -> (PreparedValidationResolutionStateV1, ValidationOneStepWitnessV1, Data)
  // the generic twin of min_ada/yield.unique_dispatch: exactly one input at a listed dispatcher credential,
  // its InlineDatum decoded as validation_semantic_v1.Datum, its Spend redeemer decoded as
  // ct.StepRedeemer<{ input_index, output_index, transition, extra: Data }>; returns (state, transition, extra)
```

Yields read `extra` and any auxiliary constructor **raw** (`builtin.un_constr_data`
tag + fields, as `canonical_action_hash_v1` does) — never `expect aux:
ValidationAuxiliaryWitnessV1`, which costs 13,353 bytes. Measured yield shell:
1,663 bytes (y00).

### 4.3 The slot-0 dispatcher (rewritten `script-sources-non-output-semantic-v1.ak`)

```aiken
pub type ActionV1 {
  VerifyNonOutput {
    input_index: Int, output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,                 // hashed into evidence as before; decoded only by the yield
    yield_role_index: Int,           // 0..7 into the compiled-in role table below
    yield_ref_input_index: Int,
  }
}
validator main(award_script_hash, computation_thread_policy_id, reference_script_auth_policy_id: PolicyId)
```

`main.spend` → `continue_winning(ScriptSources, …, auxiliary, semantic, …)` with
`semantic = { let frame = open_frame_v1(pre, transition, 30, stage_for(yield_role_index));
let yield_hash = require_semantic_yield_v1(tx, reference_script_auth_policy_id,
role_for(yield_role_index), yield_ref_input_index);
and { frame.item_count == 30, bytearray.length(yield_hash) == 28 } }` — both
bindings are used; never `let _ =` (the compiler deletes the call and its
`expect`s: discarded-binding hazard, stage-ten-match §11). The dispatcher
never decodes `auxiliary`, never builds the successor and drops
`field_preimage_certificate_policy_id` (the door moves into the two yields
that need it). Role table (Aiken constants, mirrored in
`REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`):

| idx | Role                           | Yield validator (new file under `validators/fraud-proofs/validation-trace/`) | Stage / arm                                                                      | Auxiliary (raw tag)                        |
| --: | ------------------------------ | ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------- | ------------------------------------------ |
|   0 | `V1VtSsStage2AdvanceYield`     | `script-sources-stage-two-advance-yield-v1.ak`                               | 2 → 3, install replay schedule                                                   | `NoAuxiliaryWitness` (0)                   |
|   1 | `V1VtSsStage3ReplayYield`      | `script-sources-stage-three-replay-yield-v1.ak`                              | 3, `script_sources_replay_item`                                                  | `ResolvedInputReplayWitness` (7)           |
|   2 | `V1VtSsStage3FinishYield`      | `script-sources-stage-three-finish-yield-v1.ak`                              | 3 → 4                                                                            | `NoAuxiliaryWitness`                       |
|   3 | `V1VtSsStage4BeginYield`       | `script-sources-stage-four-begin-yield-v1.ak`                                | 4, one output item via the door (field 2)                                        | `TransactionRedeemerItemBeginWitness` (29) |
|   4 | `V1VtSsStage4FinishYield`      | `script-sources-stage-four-finish-yield-v1.ak`                               | 4 → 5 (empty-outputs and complete arms)                                          | `NoAuxiliaryWitness`                       |
|   5 | `V1VtSsStage6BeginPolicyYield` | `script-sources-stage-six-begin-policy-yield-v1.ak`                          | 6, `script_sources_begin_mint_policy` (door, field 5) incl. `reject_asset_count` | `TransactionFieldChunkWitness` (1)         |
|   6 | `V1VtSsStage6FoldAssetYield`   | `script-sources-stage-six-fold-asset-yield-v1.ak`                            | 6, `script_sources_fold_mint_asset`                                              | `MintFoldAssetWitness` (39)                |
|   7 | `V1VtSsStage6FinishYield`      | `script-sources-stage-six-finish-yield-v1.ak`                                | 6 → 7 (empty-mint and complete arms)                                             | `NoAuxiliaryWitness`                       |

Each yield is `validator y(dispatcher_script_hashes: List<ScriptHash>,
field_preimage_certificate_policy_id: PolicyId /* roles 3, 5 only */) { withdraw(_, _, tx) }`:
`unique_semantic_dispatch_v1` → `(state, transition, extra)`; `open_frame_v1`
on `transition.work_witness_cbor`; raw-read the auxiliary from the dispatcher
redeemer; run the arm's semantics on raw items; build the successor by splice
and require `successor_is_exact_v1`. Concretely:

- **Stage 2 advance:** checks `items[4] > 0`, `items[14] == 0`, `items[8] == []`,
  `items[15] == initial_resolution_accumulator()`, `items[16] == empty_resolution_schedule_hash()`,
  `items[17] == 0`, `items[18] == 0`, `items[19] == []`, `items[29] != empty`;
  successor = `replace_stage_v1(3)` then `splice_v1(offset_16, enc(items[16]), enc(items[29]))`
  (offset supplied in `extra`, verified by `splice_v1`).
- **Stage 3 replay:** parse the 30 items with `script_sources_control_from_data_items`
  (2,024 B, no extension) and call the existing `script_sources_replay_item`
  unchanged — it already encodes the successor with the base encoder
  (measured p53 = 11,948 as a spend shell).
- **Stage 3 finish:** `items[16] == empty_resolution_schedule_hash()`,
  `items[14] == items[4]`, `items[15] == items[5]`; `replace_stage_v1(4)`.
- **Stage 4 begin:** `verify_native_tx_proof_source_v1` + `open_machine_field_item(door, …, 2, output_count, carriage)`
  (the arm body of `script_sources_stage_four` lines 9160–9231 unchanged);
  successor rewrites the contiguous items 21–23 (`output_count`, `output_peaks`,
  `output_total_count`) with one `splice_v1`.
- **Stage 4 finish:** `decode_native_tx_compact_v1(compact_cbor).body.outputs_hash`
  (decode only, 1,419 B — the commitment check in `open_frame_v1` binds the bytes)
  and the two arms of `script_sources_stage_four` lines 9142–9159; `replace_stage_v1(5)`.
- **Stage 6 begin policy / fold asset / finish:** bodies of
  `script_sources_begin_mint_policy`, `script_sources_fold_mint_asset` and
  the two finish arms of `script_sources_stage_six`, with successors spliced
  at items 18–19 (`purpose_count`, `purpose_peaks`) and 28 (`mint_fold`,
  re-encoded with `encode_mint_fold_control`) instead of `exact_script_sources_control`.

**Security argument.** _Dispatch uniqueness:_ `unique_semantic_dispatch_v1`
requires exactly one input at a listed dispatcher credential and exactly one
`Spend` redeemer for it, so one zero-withdrawal cannot discharge two threads.
_Role authentication:_ `require_authenticated_zero_yield` requires the indexed
reference input to carry exactly one token of `reference_script_auth_policy_id`
whose name equals the compiled-in role, an exact zero-lovelace withdrawal from
the reference script's hash, and a unique withdraw redeemer for it.
_Cross-arm substitution:_ the dispatcher maps `yield_role_index` to a fixed
role and to a fixed `expected_stage`; a stage-6 yield presented for a stage-4
witness fails at the role check, and a yield of the right role for the wrong
arm fails inside the yield (each arm re-checks its stage byte and its
arm predicate on the frame). _Output-state re-derivation:_ the yield
recomputes `claimed_successor.work_root` from the spliced bytes
(`successor_is_exact_v1`), and `continue_winning` pins the thread's output
state to `winning_resolution()` and the output script to `award_script_hash`.
_Omission:_ without the yield `require_authenticated_zero_yield` fails; with
the yield but a wrong `yield_ref_input_index` the role check fails; if the
attacker withdraws from a look-alike script the withdrawal credential does not
match the reference script hash. _What an attacker gains if a yield were
omitted from deployment:_ nothing — the corresponding arm becomes unprovable
(liveness), never provable-wrong.

## 5. Size and budget projection

| Script                         | Basis                                                                                   |                      Projected raw bytes |
| ------------------------------ | --------------------------------------------------------------------------------------- | ---------------------------------------: |
| slot-0 dispatcher              | p42 (6,326) − extension splice (~300) + role table (~250)                               |                              **≈ 6,300** |
| `V1VtSsStage2AdvanceYield`     | y00 1,663 + frame 2,392 + one `splice_v1` + arm checks                                  |                                  ≈ 5,000 |
| `V1VtSsStage3ReplayYield`      | p53 11,948 − spend shell 3,349 + yield shell 1,663 + frame 2,392                        |                                 ≈ 12,700 |
| `V1VtSsStage3FinishYield`      | y00 + frame + `replace_stage_v1`                                                        |                                  ≈ 4,600 |
| `V1VtSsStage4BeginYield`       | y00 + frame + native source 2,002 + door 3,163 + leaf/append ~800 + splice              |                                 ≈ 10,500 |
| `V1VtSsStage4FinishYield`      | y00 + frame + compact decode 1,419 + arms                                               |                                  ≈ 6,200 |
| `V1VtSsStage6BeginPolicyYield` | p54 33,561 − `exact` reach 19,571 − typed parse 2,024 + frame 2,392 − shell delta 1,686 | **≈ 12,700 (borderline; measure first)** |
| `V1VtSsStage6FoldAssetYield`   | p55 29,900 − 19,571 − 2,024 + 2,392 − 1,686                                             |                                  ≈ 9,000 |
| `V1VtSsStage6FinishYield`      | y00 + frame + compact decode + arms                                                     |                                  ≈ 6,300 |

Per semantic-resolution transaction: dispatcher + one yield = **≈ 11–19 KB
referenced** (today 115,590). Reference-script fee (Conway tiering, base 15
lovelace/byte, 25,600-byte tiers, ×1.2 — mainnet values, not pinned in this
repo): ≤ 0.29 ADA per transaction versus ≈ 2.47 ADA today (tiers 0–3 full plus
13,190 bytes in tier 4). Aggregate ExUnits: two Plutus executions parse the
spend redeemer (transition with the full work witness) — the dispatcher and
the yield — plus the yield's datum read; no measured ExUnits exist for
resolver index 8 (the fit sweep row is `unmeasuredReason: no
harness-reachable fixture … resolverIndex 8`, `demo/midgard-validation/tests/fixtures/resolver-proof-fit-sweep-v1.generated.json`),
so §7 adds the fixture that measures it against the 13,200,000 memory basis
(GOAL_SPEC §3.3). Projection method: probe deltas above; the two door yields
and stage-6 begin are the ones to re-measure before merge.

## 6. Off-chain work

Nothing named exists for this contract today beyond the `contracts.ts` title.

- **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`): add a
  `scriptSourcesNonOutputYields` record of eight `SpendingValidator`-shaped
  entries (withdraw scripts) applied by blueprint title with
  `[[dispatcher.spendingScriptHash], fieldPreimageCertificatePolicyId?]`; the
  slot-0 resolver gains `reference_script_auth_policy_id` and loses
  `field_preimage_certificate_policy_id` — both changes flow through the
  name-keyed semantic parameter map that `semantic-resolver-arity-gate.test.ts`
  guards (declare the new name, drop nothing globally: other resolvers still use the certificate policy).
- **Reference-script roles**: add the eight `V1VtSs…Yield` names to
  `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (`demo/midgard-sdk/src/reference-scripts.ts`)
  and to `midgard-core`'s `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
  (the two vocabularies must match role for role, see the comment at
  `reference-scripts.ts:221-226`); Aiken constants in
  `lib/midgard/validation-semantic-yield-v1.ak` (compare `min_ada/yield.tx_role`).
- **Manifest / deployment info**: new entries `validationTraceDisputeScriptSourcesNonOutputSemantic`
  and `validationTraceDisputeScriptSourcesStage{2Advance,3Replay,3Finish,4Begin,4Finish,6BeginPolicy,6FoldAsset,6Finish}Yield`,
  a `validationScriptSourcesSemanticReferenceScriptDeploymentEntryV1(index)`
  table in `submit.ts` mirroring `VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`,
  and `requireValidationScriptSourcesSemanticReferenceScriptUtxo`. Register
  each yield's reward account at deployment (withdraw-zero requires a
  registered script stake credential; the state-queue yields set the precedent).
- **Inspection fixtures**: `tests/inspect-contracts.test.ts` lists
  `semantic-resolver-0..90` (unchanged count) and must add
  `script-sources-yield-*` step names under `validationTraceDispute`.
- **Submit route** (`submit.ts`): `semanticActionFieldsV1` for resolver 8,
  `semanticResolverIndex === 0` emits `[input, output, transition, auxiliary,
yield_role_index, yield_ref_input_index]`; the semantic submit builder adds
  `.readFrom([yieldUtxo])` and `.withdraw(yieldRewardAddress, 0n, Data.void())`
  and resolves the yield UTxO from the deployment entry; `auxiliaryShapeV1`
  keeps its stage checks. `yield_role_index` is derived from the one-step
  argument's auxiliary tag and the control's stage byte (2→0, 3→1/2,
  4→3/4, 6→5/6/7) by a new pure helper `scriptSourcesNonOutputYieldRoleIndexV1`.
- **Funding requirements** (`demo/midgard-fault-proofs/src/workflow/funding-requirements.ts`):
  one publication row per new script (nine) plus eight 2-ADA reward-account
  deposits.
- **`midgard-core` / `validation` codec**: the one-step argument
  (`ValidationOneStepArgument`) gains `resolverHints: { yieldRoleIndex, itemOffsets: number[] }`
  filled by the evidence builder in `demo/midgard-validation` from the
  canonical work-witness encoding it already produces; `VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1`
  is unchanged.

## 7. Emulator scenario tests

Exists today: `tests/submit-init-emulator-validation-dispute.test.ts`
(publication fit for the six controls; CEK resolvers with `oversized: true`
under `maxTxSize: 262_144`), `dispute-scenario.ts` (publishes the selected
semantic resolver, `oversized` when its body exceeds `maxTxSize`), and
`buildForgedOperatorSuccessorValidationDisputeFixture` supporting only
`disputedPhase: "cek" | "valueAndMint"`
(`tests/support/emulator/validation-dispute-fixtures.ts:937`). Nothing reaches
resolver 8.

Add `tests/submit-init-emulator-script-sources-non-output-v1.test.ts` (one
journey per file, per the wasm-heap note in
`submit-init-emulator-cek-value-and-mint.test.ts`):

1. _Publication fit:_ publish the dispatcher and all eight yields with
   `publishPlainReferenceScriptUtxo` inside `withRealL1MaxTxSize`, **no
   `oversized`**, and assert `assertReferenceScriptRawBodiesFitL1EnvelopeV1`
   accepts them.
2. _Positive lifecycle through award:_ extend
   `buildForgedOperatorSuccessorValidationDisputeFixture` with
   `disputedPhase: "scriptSources"` and a `disputedStep` selector (`{ stage: 2 | 3 | 4 | 6, arm }`)
   so the bisection lands on one honest step of each arm (fixture transaction:
   `spendInputsOfCardinality` with ≥2 resolved inputs so stage 3 replays, ≥1
   output so stage 4 has an item, and a mint with two policies so stage 6
   exercises begin, fold and finish); drive
   `runForcedValidationDisputeScenario` to award and removal; assert every
   transaction `completeSignedBytes ≤ 16,384` and the semantic-resolution
   `measurement.exUnits.mem ≤ 13_200_000`.
3. _Valid-block negative at the same frontier:_ the honest operator trace
   with the same `disputedStep` must make `submitValidationDisputeSemanticResolution`
   fail (`expectOnchainRefusalV1`) because the yield's `successor_is_exact_v1`
   rejects the challenger's forged successor.
4. _Cancel/resume:_ the family supports `ct.Cancel` on every step; reuse the
   pattern of `submit-init-emulator-canonical-decodability-cancel-resume.test.ts`
   at the prepared-resolution step.
5. _Maximum shape:_ stage-4 begin with a 16,384-byte output preimage via
   tier-2 `RawUtxo` carriage (`largeFittingOutputCbor`), stage-6 fold at the
   `ledger_output_v1.max_distinct_asset_count` boundary (accept) and one over
   (`reject_asset_count` terminal).
6. Remove the `semanticIsOversized` branch in `dispute-scenario.ts` for
   resolver 8 once every script-sources plan lands.

## 8. Aiken tests

- `lib/midgard/script-sources-raw-frame-v1.test.ak`: property tests (fuzz)
  that for random well-formed `ScriptSourcesControlV1` values at stages 2, 3,
  4, 6, `open_frame_v1` accepts `exact_script_sources_control(control)` and
  rejects any single-byte mutation of the head; that each arm's splice equals
  `exact_script_sources_control(next_control)` computed by the monolith's
  path (`splice_matches_exact_encoder_stage_two_advance`, `…_stage_three_finish`,
  `…_stage_four_begin`, `…_stage_four_finish`, `…_stage_six_begin_policy`,
  `…_stage_six_fold_asset`, `…_stage_six_finish`); golden tests pinning the
  three literals (`empty_observer_scan_cbor_is_pinned`, `empty_mint_fold_cbor_is_pinned`,
  `initial_output_scan_cbor_is_pinned`).
- `validators/fraud-proofs/validation-trace/script-sources-non-output-split-v1.test.ak`
  (modelled on `value-and-mint-split-v1.test.ak`): `non_output_wire_layout_is_pinned`,
  `prepare_routes_non_output_to_slot_zero`, one `…_yield_wins_…` per arm driven
  through `main.spend` + `y.withdraw` in one `Transaction`, and negatives:
  `dispatcher_refuses_a_missing_yield`, `dispatcher_refuses_cross_arm_role_substitution`
  (stage-6 role for a stage-4 witness), `dispatcher_refuses_withdrawal_script_substitution`,
  `dispatcher_refuses_nonzero_withdrawal`, `yield_refuses_two_dispatcher_inputs`,
  `yield_refuses_a_forged_successor`, `yield_refuses_a_misplaced_splice_offset`,
  `stage_six_begin_yield_emits_reject_asset_count_terminal`.
- Keep `validation-machine-v1.test.ak` vectors for stages 2–6 as the oracle
  the splice tests compare against.

## 9. Verification commands

```bash
cd onchain/aiken && aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/script_sources_(non_output_semantic|stage_(two|three|four|six)_.*_yield)_v1\.main\.(spend|withdraw)$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(v.title,n,n<=15000?"OK":"OVER")}'
# expected: 9 titles, all OK
aiken check -m script_sources_raw_frame      # expected: ≥ 10 tests, 0 failures
aiken check -m non_output_split              # expected: ≥ 16 tests, 0 failures
cd ../../demo
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/semantic-resolver-arity-gate.test.ts tests/validation-dispute-submit.test.ts tests/inspect-contracts.test.ts
pnpm --filter @al-ft/midgard-fault-proofs test -- tests/submit-init-emulator-script-sources-non-output-v1.test.ts   # 1 journey, ≤ 900 s
pnpm --filter @al-ft/midgard-sdk test -- tests/reference-scripts.test.ts
```

## 10. Ordering and dependencies

- Lands with the other seven script-sources plans and the twelve remaining
  oversized script-sources resolvers (stages 7–12, other owners): they share
  `script-sources-raw-frame-v1.ak` and `validation-semantic-yield-v1.ak`, and
  slot-0's hash change re-applies `script_sources_v1` (prepare) and the
  catalogue root — one blueprint regeneration, one root re-pin.
- Requires the `reference_script_auth_policy_id` to be a parameter of a
  semantic resolver for the first time; the deployment must mint the eight
  role NFTs before any dispute can use them.
- Independent of the stage-one redeemer chain (slot 15/28) except for the
  shared libraries.
- The stage 7–12 plans' `validation-script-sources-yield-v1.ak`
  (stage-ten-match §4c) holds role constants and claim types only and calls
  this plan's `require_semantic_yield_v1`; there is one handshake
  implementation for the group.

## 11. Risks

- **Stage-6 begin-policy yield ≈ 12.7 KB projected**, not measured as a
  yield; if the canonical-header decoders push it over 15,000, split the
  policy header decode into a `V1VtSsStage6PolicyHeaderYield` (two yields in
  one transaction).
- **ExUnits unmeasured** for every resolver-8 step; the yield doubles redeemer
  parsing. Mitigation: the §7 fixture measures under the shared Van Rossem
  limits before merge.
- **ABI churn:** slot-0 redeemer gains two fields and drops the typed
  auxiliary; `validation-dispute-submit.test.ts` "matches the exact prepare,
  semantic, and award Aiken redeemer ABIs" must be regenerated together with
  the wire-layout pins.
- **Splice offsets are prover-supplied**; correctness relies on `splice_v1`'s
  equality check plus the canonical-by-induction argument — pinned by the
  equivalence property tests in §8, which must never be weakened to fixed
  vectors only.
- **Spec conflict check:** GOAL_SPEC §8.3 C45 requires every script-sources
  transition to remain one-step provable; the yield split keeps one machine
  step per transaction, so C52's transaction count is unchanged.
