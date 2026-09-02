# `script_sources_stage_ten_match_semantic_v1` — L1 size-fit plan

Assumes [`00-primer.md`](00-primer.md). This is the **anchor plan for the
script-sources stage 7–12 group**: it defines the three library prunes every
plan in the group reuses (§4a), the **descriptor-mode redeemer-item surface**
(§4b) and the **shared redeemer-item-step yield** `V1VtSsRedeemerItemStepYield`
consumed by this plan, `stage-ten-mismatch` and `stage-twelve-redeemer`
(§4c–4e). The sibling plans reference these sections instead of repeating them.

## 1. Identity

| Field                                                                        | Value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Blueprint title                                                              | `fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1.main.spend`                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| File                                                                         | `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-ten-match-semantic-v1.ak`                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Raw size (2026-09-01 build, re-measured 2026-09-01 in `/tmp/size-probe-ssb`) | 82,956 bytes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Applied parameters                                                           | `award_script_hash: ScriptHash`, `computation_thread_policy_id: PolicyId`                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Phase / resolver index                                                       | `ScriptSources`, resolver 8 (`VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1[8] = 29`, offset 32)                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Semantic index (arm)                                                         | 22 of 29 in the `script_sources_v1` prepare group (`prepare_selected(ScriptSources, hashes, script_sources_semantic_resolver_count = 29, …)`); global slot `validationSemanticResolverGlobalIndexV1(8, 22) = 54`                                                                                                                                                                                                                                                                                                               |
| Library entry point                                                          | `validation_machine_v1.verify_script_sources_stage_ten_match_semantics_v1` → `script_sources_stage_ten_control_from_witness` + `script_sources_stage_ten_item_matches_current_purpose` (runs `redeemer_item_proof_v1.step_v1`) → `verify_script_sources_stage_ten_semantics_v1` → `verify_script_sources_semantic_stage(…, 10, script_sources_stage_ten)` (generic `script_sources_control_from_witness` + `script_sources_control_is_bound`, then the whole three-arm `script_sources_stage_ten`, which runs `step_v1` again) |
| Redeemer action                                                              | `VerifyMatch { input_index, output_index, transition: ValidationOneStepWitnessV1, auxiliary: ValidationAuxiliaryWitnessV1 }` — the auxiliary is the full 38-constructor sum type, decoded from `Data`                                                                                                                                                                                                                                                                                                                          |
| Auxiliary accepted                                                           | `RedeemerItemStepWitness { redeemer_control: None, control: RedeemerItemProofControlV1, witness: RedeemerItemProofWitnessV1 }` (constructor 18, 3 fields — `VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep`) whose step reaches `stage_terminal` **and** whose descriptor matches the current purpose                                                                                                                                                                                                                         |
| Role name today                                                              | none — script-sources semantics are hash-checked plain reference scripts in the emulator and attach inline in production; no auth-role NFT                                                                                                                                                                                                                                                                                                                                                                                     |
| Deployment entry today                                                       | **none** — `submit.ts` has reference-script rosters only for CEK (resolver 11: `VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`) and ValueAndMint (resolver 12); resolver 8 semantics are wired in `contracts.ts` only (`VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.scriptSourcesStageTenMatch`, `demo/midgard-sdk/src/fraud-proof/contracts.ts:428`)                                                                                                                                               |

What the step proves (C45): with the discovery cursor parked on an effectful
purpose (stage 10: `matched_language_tag ∈ {3, 128}`, a redeemer item scan in
progress with `redeemer_item_control_hash != ""`), one descriptor-mode redeemer
item step (`RedeemerItemOpenHeader` or `RedeemerItemOpenTail`, chunk-proof
authenticated against the item commitment) reaches `stage_terminal`, its
`(purpose_tag, pointer_index)` equals
`redeemer_tag_for_purpose_kind_v1(current_purpose_kind), current_purpose_index`,
and the successor is the stage-8 discovery control with the purpose completed:
`execution_peaks` appended with
`execution_leaf_hash(matched_language_tag, purpose_leaf, matched_source_leaf, redeemer_item_leaf_hash(item_index, item_commitment))`,
`used_redeemer_bitmap` gaining `item_index`, `purpose_cursor + 1`, current
fields reset (`reset_script_discovery_current`).

## 2. Why it is this size

All measurements: copy of `onchain/aiken` at `/tmp/size-probe-ssb` (deleted
afterwards), pinned fork `v1.1.23-org-5adf7837`, `aiken build --env testnet`,
raw unapplied `compiledCode` bytes. Round-1/2 probes are spend validators with
one opaque redeemer `P { pre: ValidationMachineStateV1, witness: ValidationOneStepWitnessV1, auxiliary: Data, bytes }`
(floor `p00` = 1,005) calling one library function through `pub fn probe_*`
wrappers appended to the copy's `validation-machine-v1.ak` /
`redeemer-item-proof-v1.ak`. Round-3 probes are exact validator shapes.

### 2a. Decomposition of the 82,956-byte body

| Probe | Reachable code                                                                                                                          | Raw bytes |                                    Δ over floor / note |
| ----- | --------------------------------------------------------------------------------------------------------------------------------------- | --------: | -----------------------------------------------------: |
| p00   | floor: `P` redeemer decode only                                                                                                         |     1,005 |                                                      — |
| p01   | `expect a: ValidationAuxiliaryWitnessV1 = aux` (full sum-type decoder)                                                                  |    14,198 | **+13,193** — cost of typing `auxiliary` in the action |
| p02   | `script_sources_stage_ten_control_from_witness` (31-item decode + `script_discovery_control_from_cbor`)                                 |     3,619 |                                                 +2,614 |
| p09   | p02 + `native_tx_proof_commitment_v1` + `hash_validation_context`                                                                       |     3,912 |                                                   +293 |
| p03   | p02 + `script_sources_stage_ten_control_is_bound` (uses `exact_script_sources_control`)                                                 |    23,522 |                                                +19,903 |
| p04   | p02 + stage-nine-style **sliced** binding with stage-ten predicates                                                                     |     6,067 |                                                 +2,448 |
| p06   | `exact_script_sources_control(control) == cbor` alone                                                                                   |    22,489 |                                                +18,870 |
| p07   | narrow encoder alone (`encode_script_sources_witness` / `encode_script_sources_discovery_witness`)                                      |     7,252 |                                                 +3,633 |
| p08   | `ledger_output_proof_v1.encode_control_v1 ∘ decode_control_v1` (the stage-5 arm `exact_script_sources_control` drags in)                |    19,460 |                                                +18,455 |
| p05   | generic `script_sources_control_from_witness` + `script_sources_control_is_bound` (what `verify_script_sources_semantic_stage` reaches) |    33,158 |                                            **+32,153** |
| p17   | `redeemer_item_proof_v1.hash_control_v1` alone (with `RedeemerItemProofControlV1` decode incl. `Option<DataTraverseControlV1>`)         |    11,159 |                                                +10,154 |
| p15   | `redeemer_item_proof_v1.step_v1` alone                                                                                                  |    37,817 |                                            **+36,812** |
| p16   | `step_v1` + `descriptor_v1`                                                                                                             |    38,093 |                                                +37,088 |
| p12   | begin arm with sliced bound (`verify_membership`, `initial_control_v1`, `hash_control_v1`, successor)                                   |    16,156 |                                                      — |
| p13   | step arm with sliced bound (`step_v1`, `descriptor_v1`, both successors)                                                                |    47,156 |                                                      — |
| p11   | sliced bound + whole `script_sources_stage_ten`                                                                                         |    58,715 |                                                      — |
| p10   | exact bound + whole `script_sources_stage_ten`                                                                                          |    70,467 |                                                      — |

Reading: the resolver pays (i) 13.2 KB to decode the whole auxiliary sum type,
(ii) 32 KB for the _generic_ control decoder and binder — which reach the
stage-0 `inline_source_hash_control_from_cbor` (blake2b-224 trace), the stage-5
`ledger_output_proof_v1` decoder/encoder and every stage's well-formedness
predicate — and (iii) ~37 KB for `redeemer_item_proof_v1.step_v1`, whose
`control_is_well_formed`, `encode_optional_traversal` and `apply_step` reach the
entire `cek_data_traverse_v1` machine even though stage ten only ever runs the
item proof in `mode_descriptor` (header → tail → terminal; `tail_step` sets
`stage_terminal` and `traversal: None` for descriptor mode, so `stage_data` is
unreachable). `step_v1` is reached twice (family guard and stage body) but
compiled once. Stage nine fits (8,789–11,616) precisely because its resolvers
use a per-stage decoder, the sliced binding and no item-proof machine.

### 2b. Descriptor-mode surface and whole-resolver prototypes

| Probe     | Shape                                                                                                                                                                           |     Raw bytes |     Fits ≤ 15,000     |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------: | :-------------------: |
| q01       | `descriptor_step_v1` (header/tail only, descriptor well-formedness, `authenticated_span`, `head_at_v1`) with a 15-scalar control + `ChunkProofV1` decode                        |         6,767 |           —           |
| q02       | `hash_descriptor_control_v1` (prefix + `d87a80` + blake2b-256)                                                                                                                  |         3,155 |           —           |
| q03 / q04 | `bounded_item_v1.verify_chunk` / `canonical_cbor_scan_v1.head_at_v1` alone                                                                                                      | 2,963 / 1,607 |           —           |
| q06       | terminal-match step predicate, sliced bound, descriptor surface (no shell)                                                                                                      |        14,353 |           —           |
| q20       | **resolver-shaped monolith**: `cancel` + `continue_winning` + q06 predicate, narrow typed redeemer                                                                              |        16,574 |        **no**         |
| q54       | q06 as a single per-resolver yield (`withdraw`, `unique_dispatch`)                                                                                                              |        14,351 |    yes, margin 649    |
| q58       | pure dispatcher (typed claim redeemer, one `require_authenticated_zero_yield`, `continue_winning(True)`)                                                                        |         4,814 |          yes          |
| **q50**   | **dispatcher carrying stage-ten match semantics given a yield-verified claim** (sliced bound + purpose match + stage-8 successor)                                               |    **12,147** | **yes, margin 2,853** |
| **q51**   | **shared descriptor-step yield** (`withdraw`, singleton dispatcher input among 3 hashes, control decode, item-control hash check, `descriptor_step_v1`, `next == claimed_next`) |    **10,942** |        **yes**        |

## 3. Options considered

| Option                                                                                         | Verdict                                          | Reason                                                                                                                                                                                                                                                                                                                                                |
| ---------------------------------------------------------------------------------------------- | ------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1. Prune only (narrow action, per-stage decoder, sliced binding, descriptor-mode item surface) | rejected as sufficient, **kept as prerequisite** | the pruned monolith measures 16,574 (q20): over the 15,000 target and over the 16,384 envelope before parameters                                                                                                                                                                                                                                      |
| 2a. Yield: whole predicate in one per-resolver yield                                           | rejected                                         | 14,351 (q54) leaves 649 bytes; three such yields (14,351 / 14,355 / 14,086) all inside 1 KB of the target, and any library growth breaks all three at once                                                                                                                                                                                            |
| 2b. **Yield: shared descriptor-step yield + stage semantics in the dispatcher**                | **chosen**                                       | dispatcher 12,147, yield 10,942, one yield script serves three resolvers (4 scripts instead of 6), ≥ 1.6 KB margin everywhere; the shared function (`redeemer_item_proof_v1.step_v1` in descriptor mode) is exactly the one the primer asks to factor once                                                                                            |
| 2c. Yield: keep the dispatcher pure and add a second per-stage "successor" yield               | rejected                                         | 3 scripts per transaction (≈ 4.8 + 10.9 + ~12 KB) and 7 scripts to publish for no margin gain                                                                                                                                                                                                                                                         |
| 3. Chain (begin / step / match as separate computation-thread transactions)                    | rejected                                         | the single-transaction predicate is in budget today (the stage-one RF-021 chain exists because `mode_data` traversal is multi-step; descriptor mode is two item steps that the machine already exposes as separate one-step transitions); chaining would add ≥ 2 transactions per redeemer item against the C52 cap for a size problem a yield solves |
| 4. Redesign the arm boundaries (e.g. 5 resolvers: begin/advance/match/mismatch/missing)        | rejected                                         | changes `script_sources_semantic_resolver_count` (29 → 31), the prepare validator's parameter list, `VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1`, the global index table and the sweep; the yield split reaches the same sizes without touching the count                                                                                                 |

## 4. Chosen design

### 4a. Library prunes shared by the stage 7–12 group (ABI-neutral)

In `onchain/aiken/lib/midgard/validation-machine-v1.ak`:

1. **Sliced discovery-stage binding.** Add
   `fn script_sources_discovery_control_is_bound(pre, witness, control, stage: Int, stage_byte: ByteArray) -> Bool`
   generalising `script_sources_stage_nine_control_is_bound` (prefix from
   `script_sources_stage_zero_prefix_before_stage(control, 31)`, one stage byte,
   `encode_definite_bytes(encode_script_discovery_control(discovery))` suffix,
   `native_tx_proof_commitment_v1`, `hash_validation_context`, source/redeemer
   totals and frontiers, `execution_count == purpose_cursor`, bitmaps,
   `pending_source_cbor == ""`, `output_proof == None`) and have
   `script_sources_stage_{eight,ten,eleven,twelve}_control_is_bound` call it
   with their existing per-stage discovery predicates instead of
   `exact_script_sources_control(control) == witness.work_witness_cbor`.
   Soundness is the same induction stage nine relies on today: every
   discovery-stage successor (`script_discovery_successor_is_exact`) copies the
   middle bytes of the predecessor verbatim and re-encodes only the stage byte
   and the discovery suffix canonically; the entry into stage 8 is
   `script_sources_stage_seven_finish_successor_is_exact`, which encodes the
   whole control canonically (§4a.2). So any stage ≥ 8 work witness is
   canonical, and prefix + stage byte + suffix + decoded-middle predicates is
   equivalent to the exact re-encoding. Measured saving: 23,522 → 6,067 (p03 → p04).
2. **Stage-seven narrow encoder.** Add
   `fn script_sources_stage_seven_exact_control(control) -> ByteArray` =
   `encode_script_sources_witness(…30 fields…)` when `control.stage < 8`
   (`output_proof == None`, `pending_source_cbor == ""` asserted) else
   `encode_script_sources_discovery_witness(control, control.stage, control.discovery)`;
   use it in `script_sources_stage_seven_control_is_bound` and in a new
   `script_sources_stage_seven_successor_is_exact(pre, witness, next_control)`
   used by the three stage-seven successor functions. It never reaches the
   stage-0 pending-source or stage-5 output-proof encoders (p06 22,489 → p07 7,252).
3. **Descriptor-mode redeemer-item surface** (§4b) used by stages 10 and 12
   instead of `step_v1` / `hash_control_v1` / `descriptor_v1`.

`verify_script_sources` (the aggregate route behind
`verify_script_sources_one_step_v1`) and the `verify_script_sources_stage_*_semantics_v1`
generic wrappers are unchanged; the per-kind resolvers stop calling the generic
wrappers. Nothing about what any resolver proves changes; the existing
`script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly`,
`script_sources_stage_ten_redeemer_family_guards`,
`stage_ten_missing_pending_redeemer_hash_divergence_is_unreachable` and the
stage 7/8/11/12 vectors in `lib/midgard/validation-machine-v1.test.ak` guard
the refactor.

### 4b. Descriptor-mode surface in `lib/midgard/redeemer-item-proof-v1.ak`

```
pub fn descriptor_control_is_well_formed_v1(control) -> Bool   // control_is_well_formed restricted to mode_descriptor, traversal == None, stage ∈ {header, tail, terminal}
pub fn hash_descriptor_control_v1(control) -> ByteArray        // control_hash_prefix_v1(control) ++ #"d87a80" |> blake2b_256; byte-identical to hash_control_v1 for traversal == None
pub fn descriptor_step_v1(control, open_tail: Bool, chunk_proof: ChunkProofV1, next_chunk_proof: Option<ChunkProofV1>) -> Option<RedeemerItemProofStepResultV1>
                                                                 // header_step / tail_step only; the span from next_source_span_v1's header/tail cases; authenticated_span unchanged
```

`descriptor_step_v1(c, tail, p, n)` must equal
`step_v1(c, RedeemerItemProofWitnessV1 { action: if tail { RedeemerItemOpenTail } else { RedeemerItemOpenHeader }, chunk_proof: Some(p), next_chunk_proof: n })`
for every `mode_descriptor` control — a property test in §8 pins it. The
committed hash (`discovery.redeemer_item_control_hash`) therefore does not
move: the wire and evidence ABI of the machine is untouched.

### 4c. New validator list

| Validator                                                                            | Purpose                                                                                                          | File                                                                                     | Params                                                                                                                          |
| ------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `script_sources_stage_ten_match_semantic_v1.main.spend` (**dispatcher**, same title) | narrow redeemer, yield handshake, sliced stage-ten binding, purpose match, stage-8 successor, `continue_winning` | existing file                                                                            | `award_script_hash`, `computation_thread_policy_id`, **`reference_script_auth_policy_id: PolicyId`**                            |
| `script_sources_redeemer_item_step_yield_v1.main.withdraw` (**shared yield**, new)   | descriptor-mode redeemer item step for the three redeemer-scan dispatchers                                       | `validators/fraud-proofs/validation-trace/script-sources-redeemer-item-step-yield-v1.ak` | `ten_match_dispatcher_script_hash`, `ten_mismatch_dispatcher_script_hash`, `twelve_redeemer_dispatcher_script_hash: ScriptHash` |

New library module `lib/midgard/validation-script-sources-yield-v1.ak`
(mirrors `lib/midgard/fraud-proofs/min-ada/yield.ak`):

```
pub const redeemer_item_step_role: AssetName = "V1VtSsRedeemerItemStepYield"
pub const stage_seven_observer_item_role: AssetName = "V1VtSsS07ObserverItemYield"     // defined here, used by the observer plan
pub const stage_seven_observer_bound_role: AssetName = "V1VtSsS07ObserverBoundYield"

pub type RedeemerItemDescriptorControlV1 {   // the 15 scalar fields of RedeemerItemProofControlV1; traversal is implied None
  version, mode, stage, item_index, item_count, total_length: Int, item_commitment: ByteArray,
  expected_purpose_tag, expected_pointer_index, purpose_tag, pointer_index, data_offset, data_length,
  execution_memory, execution_steps: Int,
}
pub type RedeemerItemDescriptorStepClaimV1 {
  control: RedeemerItemDescriptorControlV1,
  open_tail: Bool,                              // False = RedeemerItemOpenHeader, True = RedeemerItemOpenTail
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  claimed_next: RedeemerItemDescriptorControlV1,
}
pub fn descriptor_control_v1(fields: RedeemerItemDescriptorControlV1) -> redeemer_item_proof_v1.RedeemerItemProofControlV1   // traversal: None
pub fn unique_redeemer_scan_dispatch(ten_match_hash, ten_mismatch_hash, twelve_redeemer_hash, inputs, redeemers)
  -> (ScriptHash, validation_semantic_v1.Datum, ValidationOneStepWitnessV1, RedeemerItemDescriptorStepClaimV1)
```

Types that need private machine functions live in `validation-machine-v1.ak`:

```
pub fn script_sources_redeemer_scan_control_from_witness(work_witness_cbor) -> ScriptSourcesControlV1   // 31 items, stage ∈ {10, 12}
pub fn verify_script_sources_redeemer_item_step_claim_v1(witness: ValidationOneStepWitnessV1, claim) -> Bool   // the yield predicate (§4e)
pub fn verify_script_sources_stage_ten_match_dispatch_semantics_v1(pre, witness, claim) -> Bool               // the dispatcher predicate (§4e)
```

### 4d. Redeemer ABI delta (this contract)

```
pub type ActionV1 {
  VerifyRedeemerItemStep {          // constructor 0 — identical field layout in the ten-mismatch and twelve-redeemer dispatchers
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    claim: RedeemerItemDescriptorStepClaimV1,   // NEW
    yield_to_ref_input_index: Int,              // NEW
  }
}
```

`auxiliary: ValidationAuxiliaryWitnessV1` is removed from the wire; the
auxiliary `Data` is rebuilt as
`RedeemerItemStepWitness { redeemer_control: None, control: descriptor_control_v1(claim.control), witness: RedeemerItemProofWitnessV1 { action: if claim.open_tail { RedeemerItemOpenTail } else { RedeemerItemOpenHeader }, chunk_proof: Some(claim.chunk_proof), next_chunk_proof: claim.next_chunk_proof } }`,
so the one-step **evidence hash, `prepare_semantic_resolution`, the prepare
validator and the discovery control hash are unchanged**. `chunk_proof`
becomes mandatory on the wire: a header/tail step always has a span, so
`step_v1` returned `None` for `chunk_proof: None` anyway. The datum
(`validation_semantic_v1.Datum`) and `ct.Cancel` are unchanged. The
`VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep = [18, 3]` shape check stays
valid because the auxiliary is unchanged.

### 4e. Exact handshake

Dispatcher `main.spend`, `ct.Continue(VerifyRedeemerItemStep {...})`:

1. `let yield_hash = require_authenticated_zero_yield(tx.reference_inputs, tx.withdrawals, tx.redeemers, reference_script_auth_policy_id, redeemer_item_step_role, yield_to_ref_input_index)`
   — bind and **use** the result (`bytearray.length(yield_hash) == 28` inside the
   final `and`); never `let _ =` (§11).
2. Rebuild `auxiliary_data` from `claim` as in §4d.
3. `continue_winning(ScriptSources, award_script_hash, computation_thread_policy_id, datum, input_index, output_index, transition, auxiliary_data, verify_script_sources_stage_ten_match_dispatch_semantics_v1(pre, transition, claim), own_out_ref, tx)`
   where `pre = datum.resolution.pre_state`.

`verify_script_sources_stage_ten_match_dispatch_semantics_v1` (measured as q50):

- `control = script_sources_stage_ten_control_from_witness(witness.work_witness_cbor)`;
  `script_sources_stage_ten_control_is_bound(pre, witness, control)` (now sliced, §4a.1);
- `next = claim.claimed_next`; `next.stage == redeemer_item_proof_v1.stage_terminal`;
- `redeemer_pointer_matches_purpose_v1(discovery.current_purpose_kind, discovery.current_purpose_index, next.purpose_tag, next.pointer_index)`;
- `script_discovery_successor_is_exact(pre, witness, control, 8, completed)` with
  `completed = reset_script_discovery_current(ScriptDiscoveryControlV1 { ..discovery, purpose_cursor + 1, used_redeemer_bitmap: script_discovery_bitmap_insert(bitmap, next.item_index), execution_count/peaks: append_script_execution(discovery, current_script_purpose_leaf(discovery), discovery.matched_source_leaf, discovery.matched_language_tag, script_proof_v1.redeemer_item_leaf_hash(next.item_index, next.item_commitment)) })`.

Yield `main.withdraw(_redeemer: Data, _credential, tx)` (measured as q51):

1. `unique_redeemer_scan_dispatch`: exactly one input whose payment credential
   is `Script(h)` with `h ∈ {ten_match, ten_mismatch, twelve_redeemer}` dispatcher
   hash; that input's `Spend(out_ref)` redeemer; `expect datum: Datum`;
   `expect ct.Continue(action)`; `expect VerifyRedeemerItemStep { transition, claim, .. } = action`
   (constructor 0 in all three dispatchers — §8 pins the layout).
2. `verify_script_sources_redeemer_item_step_claim_v1(transition, claim)`:
   `control = script_sources_redeemer_scan_control_from_witness(transition.work_witness_cbor)`;
   `discovery.redeemer_cursor < control.redeemer_count`;
   `discovery.redeemer_item_control_hash != ""`;
   `item_control = descriptor_control_v1(claim.control)`;
   `item_control.mode == mode_descriptor`, `item_control.item_index == discovery.redeemer_cursor`,
   `item_control.item_count == control.redeemer_count`,
   **`hash_descriptor_control_v1(item_control) == discovery.redeemer_item_control_hash`**;
   `descriptor_step_v1(item_control, claim.open_tail, claim.chunk_proof, claim.next_chunk_proof) == Some(RedeemerItemProofAdvanced { control: next })`
   and **`next == descriptor_control_v1(claim.claimed_next)`**.

### 4f. Security argument

- **Dispatch uniqueness.** The yield requires a singleton input at any of the
  three dispatcher credentials and reads that input's own `Spend` redeemer, so
  one withdrawal cannot discharge two redeemer-scan threads, and the claim it
  verifies is byte-identical to the one the dispatcher acts on (same redeemer
  `Data`). A transaction spending two dispatcher threads fails closed.
- **Role authentication.** `require_authenticated_zero_yield` needs the
  indexed reference input to carry exactly one `V1VtSsRedeemerItemStepYield`
  token under `reference_script_auth_policy_id` with `reference_script: Some(h)`,
  an exact zero-lovelace withdrawal from `Script(h)` and a unique `Withdraw`
  redeemer for `h`. Another script fails on the role token (only the
  deployment auth policy mints it, onto the published yield UTxO); another
  UTxO fails on the withdrawal credential.
- **Cross-arm substitution.** The yield attests a _stage-agnostic pure fact_
  ("this descriptor-mode item step of the committed item control yields
  `claimed_next`"); which stage semantics apply is fixed by the dispatcher
  credential the thread sits at (`prepare_selected` locked it there) and by the
  dispatcher's own stage pin (`control.stage == 10` in the sliced binding).
  The observer yields use different role names and a single-hash
  `unique_dispatch`; no other family references `redeemer_item_step_role`.
- **Output-state re-derivation.** The dispatcher, not the yield, derives and
  checks the continuation (`continue_winning`: award script hash,
  `winning_resolution()`, evidence hash, phase) and the machine successor.
  Every claim field the dispatcher consumes is bound: `claim.control` to
  `discovery.redeemer_item_control_hash` by the yield (and the discovery control
  to `pre.work_root` by the dispatcher's sliced binding of the _same_
  `transition`); `claim.claimed_next` to the authenticated chunk bytes by the
  yield's recomputation; `claim.chunk_proof`/`next_chunk_proof`/`open_tail`
  to the evidence hash by `continue_winning`.
- **If the yield is omitted:** `require_authenticated_zero_yield` fails on
  `list.at` / the token filter; no award. **If `claimed_next` is forged:** the
  yield recomputes the step and refuses. **If `claim.control` is forged:** the
  hash check against the committed discovery control refuses. **If the chunk
  proofs are forged:** `hash_one_step_evidence` differs from
  `state.evidence_hash`, so `continue_winning` refuses before semantics run,
  and `verify_chunk` in the yield refuses independently. **If a
  non-terminal step is submitted to this dispatcher:** `next.stage == stage_terminal`
  fails (that step belongs to `ten_mismatch`). **If the yield accepts a claim
  for a different thread:** impossible in the same transaction (singleton) and
  irrelevant across transactions (withdrawals are per transaction).

## 5. Size and budget projection

| Script                                                                             | Raw (measured) | Applied (≈ +73 / +110 for 3 params) | Signed publication (≈ +276; the yield also mints one role NFT) |
| ---------------------------------------------------------------------------------- | -------------: | ----------------------------------: | -------------------------------------------------------------: |
| ten-match dispatcher (q50)                                                         |         12,147 |                            ≈ 12,257 |                                ≈ 12,540 — fits, margin ≈ 3,840 |
| redeemer-item-step yield (q51; 3 explicit hash params instead of the probe's list) |         10,942 |                            ≈ 11,060 |                                                ≈ 11,340 — fits |

Total referenced script bytes in the semantic-resolution transaction:
dispatcher + yield ≈ 23,300 bytes (the thread's reference inputs today are the
resolver only, `scriptCarriage.referenceInputs`) — inside the first 25 KiB
`minFeeRefScriptCostPerByte` tier (15 lovelace/byte ≈ 0.35 ADA); ≈ 2.3 KB from
the tier boundary. Down from 82,956 referenced bytes today (tier 4).

ExUnits: **not measured** (no emulator run in this plan). Expected shape: the
descriptor step and the item-control hash run once (yield), the sliced binding
and successor once (dispatcher), plus one extra redeemer `Data` parse and the
two `list.filter`s of the handshake. Budget the yield's parse of the largest
witness once (a `ValidationOneStepWitnessV1` whose `work_witness_cbor` carries
the compact transaction, witness set and context — up to
`max_aggregate_field_preimage_bytes`-scale items — and two `ChunkProofV1`s of
`bounded_item_v1.chunk_bytes`). Today's monolith already parses the same bytes
once and additionally runs `step_v1` twice, so the split should not increase
aggregate memory. Measure with `MIDGARD_PRINT_PROOF_FIT=1` (`semanticMeasurement`
from `dispute-scenario.ts`) in the §7 test against 13,200,000 memory units (§3.3).

## 6. Off-chain work

Does not exist today for this contract: any deployment entry, submit route,
funding row or emulator publication for a resolver-8 semantic; the yield, its
role, stake registration and entry; the `reference_script_auth_policy_id`
parameter on a semantic resolver (shared with the value-and-mint plans); a
TypeScript producer of ScriptSources stage 7–12 one-step arguments (see §7).

- **SDK contracts** (`demo/midgard-sdk/src/fraud-proof/contracts.ts`): add
  `["reference_script_auth_policy_id", referenceScriptAuthPolicyId]` to
  `semanticResolverParameterValues` (name-keyed map, line ~3883 — the same
  entry the value-and-mint plans add; add once) and make
  `referenceScriptAuthPolicyId` a required input of the validation-trace
  builder. Add `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.yields.scriptSourcesRedeemerItemStep = "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main.withdraw"`
  and build `validationTraceDispute.yields.scriptSourcesRedeemerItemStep = makeWithdrawalValidator(applyBlueprintParams(blueprint, title, [semanticResolvers[54].spendingScriptHash, semanticResolvers[53].spendingScriptHash, semanticResolvers[51].spendingScriptHash]))`
  after the semantic loop (no cycle: dispatchers depend only on the role
  constant). Extend `ValidationTraceDisputeFaultProofContracts` with `yields`.
  The semantic title list and `script_sources_semantic_resolver_count = 29` are
  unchanged, so global slot 54 and `VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1` stay.
- **Arity gate** `demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts`
  derives parameters from the blueprint; passes once the name is in the map,
  fails closed with the #609 message otherwise. `zz610-compiled-script-arity.test.ts`
  sees the new withdraw validator.
- **Reference-script roles**: `"V1 validation-trace script-sources redeemer-item-step yield": "V1VtSsRedeemerItemStepYield"`
  in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (`demo/midgard-sdk/src/reference-scripts.ts`,
  next to the min-ADA yields at lines 215–216) and in
  `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
  (`demo/midgard-core/src/deployment-manifest-identity-v1.ts:740`, with its
  `deployment-manifest-identity-v1.test.ts`); the Aiken constant must equal the string.
- **Manifest / deployment info**: `demo/midgard-node/src/deployment-manifest-v1.ts`
  step-name map entry `validationTraceDisputeScriptSourcesRedeemerItemStepWithdraw`
  (precedent `fraudProofMinAdaStep02TxWithdraw`, lines 227/480);
  `demo/midgard-node/src/commands/contract-deployment-info.ts`
  `withdrawalDescriptor("validationTraceDisputeScriptSourcesRedeemerItemStepWithdraw", contracts.fraudProofContracts.validationTraceDispute.yields.scriptSourcesRedeemerItemStep, "V1 validation-trace script-sources redeemer-item-step yield")`
  (precedent lines 831–840); `demo/midgard-node/src/transactions/reference-scripts.ts`
  `manifestReferenceScriptTarget(...)` next to line 1564. **Also new for the
  whole resolver-8 group:** a `VALIDATION_SCRIPT_SOURCES_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1`
  roster in `submit.ts` keyed by semantic index (this contract: `22: "validationTraceDisputeScriptSourcesStageTenMatchSemantic"`),
  with `requireValidationScriptSourcesSemanticReferenceScriptUtxo` shaped like
  `requireValidationValueAndMintSemanticReferenceScriptUtxo` (submit.ts ~1007),
  and matching `spendDescriptor`/`manifestReferenceScriptTarget` entries, so a
  12 KB dispatcher is consumed by reference like the value-and-mint semantics
  (#634 route selection: published entry → by reference; absent → inline;
  applied body > envelope → the precise "publish it" refusal, lines ~6083–6104).
  Inspection fixtures: `inspect-contracts.test.ts` derives
  `oversizedAppliedSpendingScripts` from the applied scripts and pins
  `Q13_CATALOGUE_ROOT` — re-pin once for the whole regeneration.
- **Stake registration**: `demo/midgard-sdk/src/initialization.ts`
  `.register.Stake(scriptRewardAddress(network, …yields.scriptSourcesRedeemerItemStep.withdrawalScript))`
  beside the min-ADA yields (lines 294–320); emulator `tests/support/emulator/setup-tx.ts`
  analogue of `registerStateQueueYieldRewardAccountsV1`.
- **Submit route** (`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`):
  `semanticActionFieldsV1` (~3779, `resolverIndex === 8`) must build
  `[input_index, output_index, transition, claim, yield_to_ref_input_index]`
  for semantic 22 instead of `[...base, auxiliary]`: `claim.control` and
  `open_tail`/`chunk_proof`/`next_chunk_proof` are lifted out of the staged
  `RedeemerItemStepWitness` auxiliary (constructor 18: `[None, control, witness]`;
  `witness.action` index 0 → header, 1 → tail); `claim.claimed_next` is the
  off-chain replay of the descriptor step (the honest successor's item control;
  `validation-auxiliary-witness-v1.ts` already models `RedeemerItemProofControlV1`);
  `yield_to_ref_input_index` via `requireReferenceInputIndex` inside the
  `makeIndexedValidationStageRedeemer` layout callback (precedent
  `src/min-ada/submit-step-02-v1.ts:264`). `submitValidationDisputeSemanticResolution`
  (~5956) adds `.readFrom([yieldReferenceUtxo])` and
  `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, Data.void())`
  for semantics 19/21/22 (precedent `submit-step-02-v1.ts:297,548–568`),
  sourcing the yield UTxO from the new deployment entry. Keep the
  `redeemerScanBegin`/`redeemerItemStep` family checks at lines 1556–1566
  (the auxiliary is unchanged).
- **Funding requirements**: the validation-dispute production roster gains one
  authenticated yield publication (≈ 11.3 KB script, role NFT mint) and one
  plain dispatcher publication (≈ 12.5 KB) — same row shape as
  `fraudProofMinAdaStep02TxWithdraw` in `src/min-ada/production-workflow-v1.ts:527`.
  Today no resolver-8 publication is funded anywhere.
- **Codec**: TypeBox schemas for `RedeemerItemDescriptorControlV1` and
  `RedeemerItemDescriptorStepClaimV1` next to the ScriptSources action encoders
  (`demo/midgard-sdk/src/fraud-proof/validation-dispute.ts`); no
  `midgard-core` rejection-reason change.
- **Watcher**: `validationTraceDispute` is not installed in `demo/midgard-watcher`;
  nothing required, and the yield uses no operator-local input.

## 7. Emulator scenario tests

Exists today: `tests/submit-init-emulator-validation-dispute.test.ts` (publishes
the CEK semantics with `oversized: true` under `maxTxSize: 262_144`, lines
156–223) and `tests/support/emulator/dispute-scenario.ts:352–425`, which
publishes the selected semantic under raised parameters when
`semanticIsOversized` — every resolver-8 semantic today takes that branch.
**No emulator journey reaches any ScriptSources stage 7–12 step**: the only
trace producer (`buildHonestAcceptedNativeTransactionTraceV1`,
`tests/support/emulator/validation-dispute-fixtures.ts:701`) builds a native,
script-free transaction, `buildForgedOperatorSuccessorValidationDisputeFixture`
(line 930) accepts `disputedPhase: "cek" | "valueAndMint"` only, and neither
`demo/midgard-core/src` nor `demo/midgard-sdk/src` contains a producer of
discovery-stage controls (no `redeemer_item_control_hash` / `used_redeemer_bitmap`
outside the SDK auxiliary-shape table). `resolver-proof-fit-sweep-generate-v1.test.ts`
reports resolver-8 rows in `unfit[]` for that reason. The Aiken vectors
`script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly` and
`script_sources_stage_ten_redeemer_family_guards` are the only stage-ten fixtures.

Add `tests/submit-init-emulator-script-sources-stage-ten-match-v1.test.ts`
(one journey per file — see the wasm-heap note in
`submit-init-emulator-value-and-mint-v1.test.ts`):

- **Fixture (new, shared by the group):** a TypeScript ScriptSources
  stage 7–12 step producer (`demo/midgard-sdk/src/fraud-proof/script-sources-trace-v1.ts`)
  mirroring `script_sources_stage_seven…twelve` and
  `encode_script_sources_discovery_witness`, cross-checked against the Aiken
  vectors; an honest **effectful** transaction variant of
  `buildHonestAcceptedNativeTransactionTraceV1` (one spend from a Plutus V3
  script address with an inline source, one redeemer, no observers) so the
  trace has a stage-10 terminal-match step; `buildForgedOperatorSuccessorValidationDisputeFixture`
  gains `disputedPhase: "scriptSources"` with a `disputedStep` selector picking
  the first honest state whose control has `stage == 10 && redeemer_item_control_hash != ""`
  and whose successor is stage 8.
- **Publication fit:** publish the dispatcher plainly
  (`publishPlainReferenceScriptUtxo`, **no `oversized`**) and the yield through
  `publishAuthenticatedValidationDisputeControl` with a target
  `{ control: "script-sources redeemer-item-step yield", name: "V1 validation-trace script-sources redeemer-item-step yield" }`
  added to `validationDisputeControlPublicationTargets`
  (`tests/support/emulator/reference-scripts.ts:38–75`); both under
  `withRealL1MaxTxSize`, asserting `publicationMeasurement.l1ByteMargin > 0`;
  `buildRemovalDeploymentInfo` (`removal-deployment.ts`) gains the entries.
- **Positive lifecycle:** prepare-selected → semantic resolution (dispatcher +
  yield, `requireL1ProofEnvelope` passes) → award → removal under the shared
  Van Rossem limits (`EMULATOR_PROTOCOL_PARAMETERS`).
- **Valid-block negative at the same frontier:** operator honest, challenger
  forges `claim.claimed_next.purpose_tag` (yield refuses), omits the withdrawal
  (dispatcher refuses), or references the award's role UTxO instead of the
  yield's (role refusal); each surfaces as a local-evaluation failure, no award.
- **Cancel:** `ct.Cancel` at the dispatcher (validation-dispute has cancel, no resume).
- **Maximum supported shape:** a redeemer item whose header spans two chunks
  (`next_chunk_proof: Some`, `max_header_span = 28` straddling
  `bounded_item_v1.chunk_bytes`) at `redeemer_count = max_tx_size_derived_collection_item_count`
  frontier depth, `used_redeemer_bitmap` near `2^redeemer_count`; assert the
  signed bytes ≤ 16,384 and print `semanticMeasurement`.
- Drop the `semanticIsOversized` branch in `dispute-scenario.ts` for resolver 8
  once all twenty script-sources scripts fit.

## 8. Aiken tests

- **Library equivalence** (`lib/midgard/redeemer-item-proof-v1.test.ak`):
  `descriptor_step_agrees_with_step_v1_on_header_and_tail` (property over
  fuzzed descriptor controls and chunk proofs: `descriptor_step_v1(c, t, p, n) == step_v1(c, witness(t, p, n))`),
  `hash_descriptor_control_agrees_with_hash_control_v1`,
  `descriptor_step_refuses_data_mode_control`.
- **Library route agreement** (`lib/midgard/validation-machine-v1.test.ak`):
  keep `script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly`
  and `script_sources_stage_ten_redeemer_family_guards`; add
  `script_sources_stage_ten_match_split_agrees_with_the_aggregate`:
  `verify_script_sources_redeemer_item_step_claim_v1(w, claim) && verify_script_sources_stage_ten_match_dispatch_semantics_v1(pre, w, claim)` ⇔ `verify_script_sources(pre, w, RedeemerItemStepWitness{…}, door)` for honest and forged claims;
  `script_sources_discovery_control_is_bound_agrees_with_exact_encoding` for stages 8/10/11/12.
- **Validator vectors**, new `validators/fraud-proofs/validation-trace/script-sources-redeemer-scan-split-v1.test.ak`
  (pattern: `value-and-mint-split-v1.test.ak`, `cek-split-v1.test.ak`):
  `ten_match_wire_layout_is_pinned` (constructor 0 field order shared with
  mismatch/twelve), `ten_match_dispatcher_wins_with_authenticated_yield`
  (fixture transaction with the yield reference input carrying
  `V1VtSsRedeemerItemStepYield`, a zero withdrawal and a unique withdraw
  redeemer); negatives (`fail`): `_refuses_missing_yield_reference_input`,
  `_refuses_cross_arm_role_token` (`V1VtSsS07ObserverItemYield`, `V1FpMinAdaS02TxYield`),
  `_refuses_withdrawal_script_substitution`, `_refuses_nonzero_withdrawal`,
  `_refuses_non_terminal_claimed_next`, `_refuses_purpose_mismatch` (that step
  belongs to ten-mismatch), `_refuses_forged_execution_leaf`.
- **Yield vectors** in the same file: `redeemer_item_step_yield_accepts_honest_header_step`,
  `_accepts_honest_tail_step`, `_refuses_forged_claimed_next`,
  `_refuses_item_control_not_committed_by_discovery`,
  `_refuses_two_dispatcher_inputs`, `_refuses_foreign_dispatcher_credential`,
  `_refuses_data_mode_control`, `_refuses_missing_chunk_proof_span`.

## 9. Verification commands

```bash
# 1. Sizes (run in a copy, never in the checkout)
cp -r onchain/aiken /tmp/size-check-ssb && cd /tmp/size-check-ssb
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/script_sources_(stage_(seven|eight|ten|eleven|twelve)_[a-z_]+_semantic_v1\.main\.spend|redeemer_item_step_yield_v1\.main\.withdraw|stage_seven_observer_(item|bound)_yield_v1\.main\.withdraw)$/.test(v.title)){const n=Buffer.from(v.compiledCode,"hex").length;console.log(n<=15000?"ok ":"BIG",n,v.title)}'
# expect 15 lines, all "ok": twelve semantics (ten_match ≈12,147) + 3 yields (redeemer_item_step ≈10,942)
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m script_sources   # all pass; the 11 existing stage 7–12 vectors + the new ones
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check -m redeemer_item     # 3 existing + 3 equivalence properties
cd - && rm -rf /tmp/size-check-ssb

# 2. TypeScript (from demo/midgard-fault-proofs, pinned Node 22.22.2)
pnpm exec vitest run tests/zz605-semantic-resolver-arity.test.ts tests/zz610-compiled-script-arity.test.ts tests/validation-dispute-submit.test.ts tests/inspect-contracts.test.ts
pnpm exec vitest run tests/submit-init-emulator-script-sources-stage-ten-match-v1.test.ts   # publication margins > 0, award + removal
# from demo/midgard-core
pnpm exec vitest run tests/deployment-manifest-identity-v1.test.ts
```

## 10. Ordering and dependencies

- Lands with the other nineteen script-sources plans in the one blueprint
  regeneration: all 29 hashes feed `script_sources_v1`'s
  `semantic_resolver_script_hashes` (count unchanged), which re-applies up the
  family to the catalogue root.
- **§4a prunes** are shared by every stage 7–12 plan; **§4b** is shared with
  `stage-ten-mismatch` and `stage-twelve-redeemer`, and is independent of the
  RF-021 stage-one chain (which runs the item proof in `mode_data` and keeps
  using `step_v1` through `script_sources_redeemer_normalization_v1`).
- The **redeemer-item-step yield (§4c–4e)** must land first among the three;
  its three parameters are those dispatchers' hashes and the dispatchers depend
  only on `redeemer_item_step_role`.
- The `reference_script_auth_policy_id` semantic-parameter name is the one the
  value-and-mint plans add; add it once. The observer plan's two yields use the
  same module `validation-script-sources-yield-v1.ak`.
- Other groups' script-sources plans (`non-output`, `output-proof-*`,
  `stage-zero-begin`, `stage-one-finish`, `stage-one-redeemer`) touch the
  _generic_ `script_sources_control_is_bound` / `exact_script_sources_control`;
  this group stops calling them from stage 7–12 resolvers and does not edit them.
- **Ownership of the redeemer-item machine (reconciled with the stage-one
  and CEK plans).** Two decompositions of `redeemer_item_proof_v1.step_v1`
  exist and are disjoint by mode. The stage-one-redeemer plan
  ([script-sources A](validation-trace-script-sources-stage-one-redeemer-semantic-v1.md)
  §4.2, §10) owns the **`mode_data`** machine — every
  `RedeemerItemProofActionV1` family as RF-021 chain executors — and is the
  only decomposition the CEK context plan consumes (its stage 0/9 item steps
  run in `mode_data`). This plan owns the **`mode_descriptor`** two-step
  surface (§4b) and the single-transaction `V1VtSsRedeemerItemStepYield`,
  consumed by `ten-mismatch` and `twelve-redeemer` only; it is not usable by
  the CEK chain (descriptor mode, ScriptSources-credential dispatch). The
  header/tail logic lives once in `redeemer_item_proof_v1`
  (`header_step` / `tail_step`): `descriptor_step_v1` is their
  `mode_descriptor` specialisation and the A chain's open-header /
  open-tail executors call the same functions in `mode_data`.
  `hash_descriptor_control_v1` (§4b) is the one hash for a control with
  `traversal == None`; the stage-one begin template `initial_control_hash_v1`
  and the CEK `hash_descriptor_control_v1` callers are pinned equal to it, not
  defined separately.
- **One handshake implementation.** `validation-script-sources-yield-v1.ak`
  (§4c) holds role constants, claim types and the dispatch lookups only; the
  `require_authenticated_zero_yield` call in §4e goes through the non-output
  plan's `require_semantic_yield_v1`
  ([non-output §4.2](validation-trace-script-sources-non-output-semantic-v1.md)),
  the same wrapper every other script-sources yield uses.

## 11. Risks

- **Budget:** ExUnits unmeasured; the extra redeemer parse in the yield is the
  only new work. Mitigation: the §7 max-shape assertion against 13,200,000 memory units.
- **ABI churn:** new action layout and third parameter on three resolvers, a
  new withdraw validator, role, deployment entries for a phase that had none;
  the SDK action encoder for semantics 19/21/22 changes. Evidence hash,
  discovery control hash and prepare are unchanged, which bounds the churn to
  these three redeemers.
- **Induction assumption of the sliced binding:** sound only while every
  discovery-stage successor preserves the middle bytes verbatim and the
  stage-7 finish successor encodes canonically; the
  `script_sources_discovery_control_is_bound_agrees_with_exact_encoding`
  property is the guard. It is the assumption stage nine already makes.
- **Compiler hazards:** the pinned fork has aborted without diagnostics on some
  constructs (value-and-mint plan §11); none appeared in the three probe
  rounds here, but keep `when` over `== None` and typed `let` before `Data` upcasts.
- **Discarded-binding hazard:** `let _x = require_authenticated_zero_yield(...)`
  deletes the call and its `expect`s; bind the hash and use it.
- **Spec:** C45 semantics unchanged (same predicate across two scripts in one
  transaction); C52 unaffected (no added transactions); C53/§3.3 byte fit is
  the done criterion; the resolver count stays 29 so `select_semantic_resolver`
  is untouched.
