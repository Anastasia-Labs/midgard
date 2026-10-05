//// One-successor vectors for the Data walker's head steps.
////
//// The award rule checks only the challenger's claimed successor, so every
//// step must admit exactly one successor from a given pre-state. A head step
//// reads its item from the authenticated head window: the scalar's encoding
//// length, a sequence's close position and a large constructor's index length
//// and field count are all fixed by those bytes, so the head actions carry no
//// arguments.
////
//// Each vector enumerates every action encoding a prover can present for one
//// head kind (the argument-free constructor and the same constructor with
//// integer arguments) and collects the distinct successors that are admitted
//// from one pre-state:
////
//// - by the walker itself (`cek_data_traverse_v1.step_v1`);
//// - on ScriptSources stage 1 (the redeemer item proof): by the monolithic
////   one-step verifier, by the raw split envelope and by the split head
////   executor;
//// - on the ledger-output-proof datum walk: by the monolithic ScriptSources
////   verifier together with its split step semantics and the narrow head
////   yield, and by the ResolveInputs pending-proof verifier.
////
//// Each set has exactly one member. The candidates are built as Data and
//// decoded with a soft cast, so a candidate the action type does not admit
//// simply yields no successor.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
  encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardExecutionUnits, MidgardRedeemerWitness,
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, MidgardTxOutput, MidgardValue, ScriptCredential, SpendRedeemer,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_datum as narrow_datum
use midgard/ledger_output_proof_raw as lraw
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1 as lop
use midgard/redeemer_item_proof_v1 as item
use midgard/script_proof_v1
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_item_semantics as semantics
use midgard/script_sources_redeemer_normalization_v1 as envelope
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/resolve_inputs as vm_resolve_inputs
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, validation_context_cbor,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, inline_field_door, one_step_evidence,
  resolve_inputs_step_fixture, script_sources_active_output_proof_fixture,
  script_sources_pending_redeemer_item_fixture, signer_frontier_for_tx,
  singleton_field_item_chunk_proof, singleton_root, submitted_source,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Candidate head actions
// ---------------------------------------------------------------------------

const head_scalar_tag: Int = 1

const head_sequence_tag: Int = 2

const head_large_constructor_tag: Int = 4

/// The action a candidate encoding decodes to, if the action type admits it.
fn as_action(candidate: Data) -> Option<traverse.DataTraverseActionV1> {
  if candidate is action: traverse.DataTraverseActionV1 {
    Some(action)
  } else {
    None
  }
}

/// Every encoding of a head action a prover can present: the argument-free
/// constructor, and the constructor carrying one integer argument in
/// `[0, bound]` or (for the large constructor) two.
fn candidates(tag: Int, bound: Int) -> List<Data> {
  let range = list.range(0, bound)
  let one_argument = list.map(
    range,
    fn(n) { builtin.constr_data(tag, [builtin.i_data(n)]) },
  )
  let two_arguments = list.flat_map(
    list.range(0, 3),
    fn(first) {
      list.map(
        range,
        fn(second) {
          builtin.constr_data(
            tag,
            [builtin.i_data(first), builtin.i_data(second)],
          )
        },
      )
    },
  )
  [builtin.constr_data(tag, []), ..list.concat(one_argument, two_arguments)]
}

fn admitted_actions(tag: Int, bound: Int) -> List<traverse.DataTraverseActionV1> {
  list.filter_map(candidates(tag, bound), as_action)
}

/// The honest action of a head kind: the argument-free constructor when the
/// action type admits it, else the constructor with the honest arguments.
fn honest_action(
  tag: Int,
  honest_arguments: List<Data>,
) -> traverse.DataTraverseActionV1 {
  when as_action(builtin.constr_data(tag, [])) is {
    Some(action) -> action
    None -> {
      expect Some(action) = as_action(
        builtin.constr_data(tag, honest_arguments),
      )
      action
    }
  }
}

fn distinct_count(successors: List<ByteArray>) -> Int {
  list.length(list.unique(successors))
}

// ---------------------------------------------------------------------------
// The walker
// ---------------------------------------------------------------------------

/// The head window the walker reads next, sliced from the item's own bytes.
fn walker_window(
  control: traverse.DataTraverseControlV1,
  source: ByteArray,
) -> Option<ByteArray> {
  when traverse.next_source_span_v1(control) is {
    None -> None
    Some(span) ->
      Some(
        builtin.slice_bytearray(
          span.absolute_start - control.source_start,
          span.length,
          source,
        ),
      )
  }
}

fn walker_step(
  control: traverse.DataTraverseControlV1,
  source: ByteArray,
  action: traverse.DataTraverseActionV1,
) -> Option<traverse.DataTraverseControlV1> {
  traverse.step_v1(control, walker_window(control, source), action)
}

fn walker_successors(
  control: traverse.DataTraverseControlV1,
  source: ByteArray,
  tag: Int,
  bound: Int,
) -> List<ByteArray> {
  list.filter_map(
    admitted_actions(tag, bound),
    fn(action) {
      when walker_step(control, source, action) is {
        Some(next) -> Some(traverse.encode_control_v1(next))
        None -> None
      }
    },
  )
}

fn walker_root(source: ByteArray) -> traverse.DataTraverseControlV1 {
  traverse.initial_control_v1(0, bytearray.length(source))
}

/// The walker pre-state at the first child of the constructor `d8799f..ff`.
fn walker_first_child(
  source: ByteArray,
  honest_count: Int,
) -> traverse.DataTraverseControlV1 {
  expect Some(opened) = walker_step(
    walker_root(source),
    source,
    honest_action(head_sequence_tag, [builtin.i_data(honest_count)]),
  )
  expect opened.stage == traverse.stage_head
  opened
}

/// One successor per walker pre-state, for a head kind: exactly one distinct
/// successor, and it exists.
fn walker_one_successor(
  control: traverse.DataTraverseControlV1,
  source: ByteArray,
  tag: Int,
  bound: Int,
) -> Bool {
  distinct_count(walker_successors(control, source, tag, bound)) == 1
}

/// `1864` (100): the scalar's length is its head's.
test walker_head_scalar_top_level_admits_one_successor() {
  walker_one_successor(walker_root(#"1864"), #"1864", head_scalar_tag, 4)
}

/// `d8799f 01 ff`: the child `01` cannot swallow the authenticated break.
test walker_head_scalar_nested_admits_one_successor() {
  let source = #"d8799f01ff"
  walker_one_successor(
    walker_first_child(source, 1),
    source,
    head_scalar_tag,
    4,
  )
}

/// `d8799f 01 ff`: the sequence's close position is the authenticated break.
test walker_head_sequence_constructor_admits_one_successor() {
  let source = #"d8799f01ff"
  walker_one_successor(walker_root(source), source, head_sequence_tag, 8)
}

/// `9f 00 00 ff`: an indefinite list closes where its break is.
test walker_head_sequence_indefinite_list_admits_one_successor() {
  let source = #"9f0000ff"
  walker_one_successor(walker_root(source), source, head_sequence_tag, 8)
}

/// `d8 66 82 18c8 80` (Constr 200 []): the index length and the field count
/// are read from the index head and the field list header.
test walker_head_large_constructor_empty_admits_one_successor() {
  let source = #"d8668218c880"
  walker_one_successor(
    walker_root(source),
    source,
    head_large_constructor_tag,
    6,
  )
}

/// `d8 66 82 18c8 9f 01 ff` (Constr 200 [1]).
test walker_head_large_constructor_indefinite_admits_one_successor() {
  let source = #"d8668218c89f01ff"
  walker_one_successor(
    walker_root(source),
    source,
    head_large_constructor_tag,
    6,
  )
}

/// Honest controls: the empty list, and a chunked byte string longer than one
/// 64-byte chunk, each still admit their one successor.
test walker_honest_controls_admit_one_successor() {
  let chunked = bytearray.concat(
    #"5f5840",
    bytearray.concat(
      bytearray.from_string(
        @"jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj",
      ),
      #"416aff",
    ),
  )
  and {
    walker_one_successor(walker_root(#"80"), #"80", head_sequence_tag, 8),
    walker_one_successor(
      walker_root(chunked),
      chunked,
      head_scalar_tag,
      bytearray.length(chunked) + 2,
    ),
  }
}

// ---------------------------------------------------------------------------
// ScriptSources stage 1: the redeemer item proof
// ---------------------------------------------------------------------------

fn tx_with_redeemer(data: ByteArray) -> Pair<MidgardTransaction, ByteArray> {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: data,
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  Pair(
    MidgardTransaction {
      ..base,
      witness_set: MidgardTransactionWitnessSet {
        ..base.witness_set,
        redeemer_tx_wits: [redeemer],
      },
    },
    encode_midgard_redeemer_witness(redeemer),
  )
}

fn pending(
  tx: MidgardTransaction,
  control: item.RedeemerItemProofControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_pending_redeemer_item_fixture(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    item.hash_control_v1(control),
    program_counter,
  )
}

fn item_witness(
  action: item.RedeemerItemProofActionV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> item.RedeemerItemProofWitnessV1 {
  item.RedeemerItemProofWitnessV1 {
    action,
    chunk_proof: Some(proof),
    next_chunk_proof: None,
  }
}

fn data_witness(
  action: traverse.DataTraverseActionV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> item.RedeemerItemProofWitnessV1 {
  item_witness(item.RedeemerItemTraverseData { action }, proof)
}

/// The monolithic stage-1 one-step verifier over `control -> next`.
fn mono_accepts(
  tx: MidgardTransaction,
  control: item.RedeemerItemProofControlV1,
  witness: item.RedeemerItemProofWitnessV1,
  next: item.RedeemerItemProofControlV1,
  program_counter: Int,
) -> Bool {
  let Pair(pre, pre_cbor) = pending(tx, control, program_counter)
  let Pair(post, _) = pending(tx, next, program_counter + 1)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      pre_cbor,
      post,
      vm_machine_types.RedeemerItemStepWitness {
        redeemer_control: None,
        control,
        witness,
      },
    ),
    inline_field_door(),
  )
}

/// The stage-1 raw split envelope, given the claimed next item-control hash.
fn envelope_accepts(
  tx: MidgardTransaction,
  control: item.RedeemerItemProofControlV1,
  witness: item.RedeemerItemProofWitnessV1,
  next: item.RedeemerItemProofControlV1,
  program_counter: Int,
) -> Bool {
  let Pair(pre, pre_cbor) = pending(tx, control, program_counter)
  let Pair(post, _) = pending(tx, next, program_counter + 1)
  let auxiliary: Data = vm_machine_types.RedeemerItemStepWitness {
    redeemer_control: None,
    control,
    witness,
  }
  envelope.verify_raw_envelope_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: pre_cbor,
      claimed_successor: post,
    },
    auxiliary,
    item.hash_control_v1(next),
    envelope.head_family,
  ) != None
}

/// The split head executor of `tag` over the authenticated source window.
fn split_accepts(
  control: item.RedeemerItemProofControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  action: traverse.DataTraverseActionV1,
  next: item.RedeemerItemProofControlV1,
  tag: Int,
) -> Bool {
  expect Some(Some(source_bytes)) = item.authenticate_source_window(
    control,
    item.prevalidated_next_source_span(control),
    data_witness(action, proof),
  )
  expect Some(current) = control.traversal
  expect Some(next_traversal) = next.traversal
  let execution = normalized.TraversalExecution {
    output: normalized.ExecutionOutput {
      deployment_id: #"",
      family: envelope.head_family,
      executor_script_hash: #"",
      settlement_script_hash: #"",
      attestation: 0,
    },
    current,
    next: next_traversal,
    action,
    source_bytes: Some(source_bytes),
  }
  if tag == head_scalar_tag {
    semantics.head_scalar(execution)
  } else if tag == head_sequence_tag {
    semantics.head_sequence(execution)
  } else {
    semantics.head_large_constructor(execution)
  }
}

/// The item control at the data stage, its walker at the root head. The
/// stage-1 begin that binds this item is covered by the stage-1 suites; here
/// the honest prefix is stepped directly.
fn redeemer_data_stage(
  redeemer_cbor: ByteArray,
) -> Pair<item.RedeemerItemProofControlV1, bounded_item_v1.ChunkProofV1> {
  let proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let initial = item.initial_control_v1(
    item.mode_data,
    0,
    1,
    bytearray.length(redeemer_cbor),
    bounded_item_v1.from_bytes(8, 0, redeemer_cbor),
    -1,
    -1,
  )
  let tail = item_advanced(
    initial,
    item_witness(item.RedeemerItemOpenHeader, proof),
  )
  let data = item_advanced(tail, item_witness(item.RedeemerItemOpenTail, proof))
  expect data.stage == item.stage_data
  expect Some(traversal) = data.traversal
  expect traversal.stage == traverse.stage_head
  Pair(data, proof)
}

fn item_advanced(
  control: item.RedeemerItemProofControlV1,
  witness: item.RedeemerItemProofWitnessV1,
) -> item.RedeemerItemProofControlV1 {
  expect Some(item.RedeemerItemProofAdvanced { control: next }) = item.step_v1(
    control,
    witness,
  )
  next
}

/// The item control at the first child of a `d8799f..ff` redeemer.
fn redeemer_first_child(
  redeemer_cbor: ByteArray,
  honest_count: Int,
) -> Pair<item.RedeemerItemProofControlV1, bounded_item_v1.ChunkProofV1> {
  let Pair(data, proof) = redeemer_data_stage(redeemer_cbor)
  let opened = item_advanced(
    data,
    data_witness(
      honest_action(head_sequence_tag, [builtin.i_data(honest_count)]),
      proof,
    ),
  )
  expect Some(walker) = opened.traversal
  expect walker.stage == traverse.stage_head
  Pair(opened, proof)
}

/// Distinct successors of one stage-1 head pre-state admitted by `accepts`
/// (the monolithic verifier, the raw split envelope or the split executor).
/// Each verifier runs in its own test, so each test program stays small.
fn redeemer_successors(
  control: item.RedeemerItemProofControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  tag: Int,
  bound: Int,
  accepts: fn(traverse.DataTraverseActionV1, item.RedeemerItemProofControlV1) ->
    Bool,
) -> Int {
  distinct_count(
    list.filter_map(
      admitted_actions(tag, bound),
      fn(action) {
        when item.step_v1(control, data_witness(action, proof)) is {
          Some(item.RedeemerItemProofAdvanced { control: next }) ->
            if accepts(action, next) {
              Some(item.hash_control_v1(next))
            } else {
              None
            }
          _ -> None
        }
      },
    ),
  )
}

fn by_monolith(
  tx: MidgardTransaction,
  at: Pair<item.RedeemerItemProofControlV1, bounded_item_v1.ChunkProofV1>,
  tag: Int,
  bound: Int,
) -> Int {
  let Pair(control, proof) = at
  redeemer_successors(
    control,
    proof,
    tag,
    bound,
    fn(action, next) {
      mono_accepts(tx, control, data_witness(action, proof), next, 13)
    },
  )
}

fn by_envelope(
  tx: MidgardTransaction,
  at: Pair<item.RedeemerItemProofControlV1, bounded_item_v1.ChunkProofV1>,
  tag: Int,
  bound: Int,
) -> Int {
  let Pair(control, proof) = at
  redeemer_successors(
    control,
    proof,
    tag,
    bound,
    fn(action, next) {
      envelope_accepts(tx, control, data_witness(action, proof), next, 13)
    },
  )
}

fn by_executor(
  at: Pair<item.RedeemerItemProofControlV1, bounded_item_v1.ChunkProofV1>,
  tag: Int,
  bound: Int,
) -> Int {
  let Pair(control, proof) = at
  redeemer_successors(
    control,
    proof,
    tag,
    bound,
    fn(action, next) { split_accepts(control, proof, action, next, tag) },
  )
}

/// Top-level `1864`.
test redeemer_head_scalar_top_level_monolith_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"1864")
  by_monolith(tx, redeemer_data_stage(redeemer_cbor), head_scalar_tag, 4) == 1
}

test redeemer_head_scalar_top_level_envelope_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"1864")
  by_envelope(tx, redeemer_data_stage(redeemer_cbor), head_scalar_tag, 4) == 1
}

test redeemer_head_scalar_top_level_executor_admits_one_successor() {
  let Pair(_, redeemer_cbor) = tx_with_redeemer(#"1864")
  by_executor(redeemer_data_stage(redeemer_cbor), head_scalar_tag, 4) == 1
}

/// Nested `d8799f 1864 ff`: the child scalar after the honest open.
test redeemer_head_scalar_nested_monolith_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8799f1864ff")
  by_monolith(tx, redeemer_first_child(redeemer_cbor, 1), head_scalar_tag, 4) == 1
}

test redeemer_head_scalar_nested_envelope_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8799f1864ff")
  by_envelope(tx, redeemer_first_child(redeemer_cbor, 1), head_scalar_tag, 4) == 1
}

test redeemer_head_scalar_nested_executor_admits_one_successor() {
  let Pair(_, redeemer_cbor) = tx_with_redeemer(#"d8799f1864ff")
  by_executor(redeemer_first_child(redeemer_cbor, 1), head_scalar_tag, 4) == 1
}

/// `d8799f 01 ff` at the root: one close position.
test redeemer_head_sequence_monolith_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8799f01ff")
  by_monolith(tx, redeemer_data_stage(redeemer_cbor), head_sequence_tag, 8) == 1
}

test redeemer_head_sequence_envelope_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8799f01ff")
  by_envelope(tx, redeemer_data_stage(redeemer_cbor), head_sequence_tag, 8) == 1
}

test redeemer_head_sequence_executor_admits_one_successor() {
  let Pair(_, redeemer_cbor) = tx_with_redeemer(#"d8799f01ff")
  by_executor(redeemer_data_stage(redeemer_cbor), head_sequence_tag, 8) == 1
}

/// `d8 66 82 18c8 80` at the root: one index length and one field count.
test redeemer_head_large_constructor_monolith_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8668218c880")
  by_monolith(
    tx,
    redeemer_data_stage(redeemer_cbor),
    head_large_constructor_tag,
    6,
  ) == 1
}

test redeemer_head_large_constructor_envelope_admits_one_successor() {
  let Pair(tx, redeemer_cbor) = tx_with_redeemer(#"d8668218c880")
  by_envelope(
    tx,
    redeemer_data_stage(redeemer_cbor),
    head_large_constructor_tag,
    6,
  ) == 1
}

test redeemer_head_large_constructor_executor_admits_one_successor() {
  let Pair(_, redeemer_cbor) = tx_with_redeemer(#"d8668218c880")
  by_executor(redeemer_data_stage(redeemer_cbor), head_large_constructor_tag, 6) == 1
}

// ---------------------------------------------------------------------------
// The ledger-output-proof datum walk
// ---------------------------------------------------------------------------

fn datum_output(datum: ByteArray) -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: Some(datum),
    script_ref: None,
  }
}

fn advanced(
  result: Option<lop.LedgerOutputProofStepResultV1>,
) -> lop.LedgerOutputProofControlV1 {
  expect Some(lop.LedgerOutputProofAdvanced { control }) = result
  control
}

fn attach_witness(
  proof: bounded_item_v1.ChunkProofV1,
) -> lop.LedgerOutputProofWitnessV1 {
  lop.LedgerOutputProofSpanAttach { chunk_proof: proof, next_chunk_proof: None }
}

fn recorded_window_bytes(
  control: lop.LedgerOutputProofControlV1,
  output_cbor: ByteArray,
) -> ByteArray {
  expect Some(window) = control.span_window
  bytearray.slice(output_cbor, window.start, window.start + window.length - 1)
}

/// The honest driver up to `target`: span attach when admitted, else the
/// witness-free step, else the recorded window, else the chunk proofs.
fn drive_to(
  control: lop.LedgerOutputProofControlV1,
  output_cbor: ByteArray,
  proof: bounded_item_v1.ChunkProofV1,
  target: Int,
  fuel: Int,
) -> lop.LedgerOutputProofControlV1 {
  expect fuel > 0
  if control.stage == target {
    control
  } else {
    let next = when lop.step_v1(control, attach_witness(proof)) is {
      Some(lop.LedgerOutputProofAdvanced { control: attached }) -> attached
      _ ->
        when lop.step_v1(control, lop.LedgerOutputProofNoWitness) is {
          Some(lop.LedgerOutputProofAdvanced { control: plain }) -> plain
          _ -> {
            let chunks = lop.LedgerOutputProofChunks {
              chunk_proof: proof,
              next_chunk_proof: None,
            }
            when control.span_window is {
              None -> advanced(lop.step_v1(control, chunks))
              Some(_) ->
                when
                  lop.step_v1(
                    control,
                    lop.LedgerOutputProofWindow {
                      bytes: recorded_window_bytes(control, output_cbor),
                    },
                  )
                is {
                  Some(lop.LedgerOutputProofAdvanced { control: consumed }) ->
                    consumed
                  _ -> advanced(lop.step_v1(control, chunks))
                }
            }
          }
        }
    }
    drive_to(next, output_cbor, proof, target, fuel - 1)
  }
}

/// The datum walk at a head step, with the window it reads recorded.
fn with_head_window(
  control: lop.LedgerOutputProofControlV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> lop.LedgerOutputProofControlV1 {
  when lop.step_v1(control, attach_witness(proof)) is {
    Some(lop.LedgerOutputProofAdvanced { control: attached }) -> attached
    _ -> control
  }
}

fn datum_witness(
  control: lop.LedgerOutputProofControlV1,
  output_cbor: ByteArray,
  action: traverse.DataTraverseActionV1,
) -> lop.LedgerOutputProofWitnessV1 {
  lop.LedgerOutputProofDatum {
    action,
    window: Some(recorded_window_bytes(control, output_cbor)),
  }
}

/// The datum walk's root head pre-state.
fn datum_root(
  output_cbor: ByteArray,
  proof: bounded_item_v1.ChunkProofV1,
) -> lop.LedgerOutputProofControlV1 {
  let ready = drive_to(
    lop.initial_control_v1(
      0,
      bytearray.length(output_cbor),
      bounded_item_v1.from_bytes(2, 0, output_cbor),
    ),
    output_cbor,
    proof,
    lop.stage_datum_traversal,
    30,
  )
  with_head_window(ready, proof)
}

fn tx_producing(output: MidgardTxOutput) -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
}

/// The ScriptSources claimed successor when the monolithic verifier, the
/// split step semantics and the narrow head yield all admit it.
fn ss_successor(
  output: MidgardTxOutput,
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
  narrow_step: fn(lraw.Frame, Data) -> Option<narrow.StepResult>,
) -> Option<ByteArray> {
  let output_cbor = encode_midgard_tx_output(output)
  let tx = tx_producing(output)
  when lop.step_v1(before, witness) is {
    Some(lop.LedgerOutputProofAdvanced { control: after }) -> {
      let frontier = validation_merkle_v1.build_frontier(
        [script_proof_v1.output_leaf_hash(0, output_cbor)],
      )
      let scan = vm_witness.empty_receive_purpose_scan_control()
      let Pair(pre, work_witness_cbor) = script_sources_active_output_proof_fixture(
        tx,
        0,
        frontier,
        scan,
        before,
        17,
      )
      let Pair(post, _) = script_sources_active_output_proof_fixture(
        tx,
        0,
        frontier,
        scan,
        after,
        18,
      )
      let transition = vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      }
      let witness_data: Data = witness
      let narrow_admits = when
        narrow_step(lraw.open(lop.encode_control_v1(before)), witness_data)
      is {
        Some(narrow.Advanced { control: raw_next }) ->
          lraw.encode(raw_next) == lop.encode_control_v1(after)
        _ -> False
      }
      if and {
        vm_script_sources.verify_script_sources(
          pre,
          transition,
          vm_machine_types.LedgerOutputProofStepWitness { witness },
          inline_field_door(),
        ),
        vm_script_sources.verify_script_sources_output_proof_step_semantics_v1(
          pre,
          transition,
          witness,
        ),
        narrow_admits,
      } {
        Some(post.work_root)
      } else {
        None
      }
    }
    _ -> None
  }
}

fn ri_state(
  tx: MidgardTransaction,
  base: validation_trace_v1.ValidationMachineStateV1,
  key: ByteArray,
  descriptor_cbor: ByteArray,
  output_proof: lop.LedgerOutputProofControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let schedule = vm_witness.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    vm_witness.initial_resolution_accumulator(),
    schedule,
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    Some(
      vm_machine_types.ResolveInputOutputProofV1 {
        source_kind: 0,
        key,
        next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
        descriptor_cbor,
        output_proof,
      },
    ),
    schedule,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..base,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ResolveInputs,
        program_counter,
        cbor,
      ),
    },
    cbor,
  )
}

/// A well-formed descriptor of the output `control` walks, MPF-pinned at
/// begin. Its facts are proven only by the finalize, so the head step is
/// checked over this pinned descriptor.
fn pinned_descriptor(control: lop.LedgerOutputProofControlV1) -> ByteArray {
  let summary = cek_data_v1.DataSummaryV1 {
    root: control.item_commitment,
    cbor_length: 1,
    memory: 1,
  }
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index: control.output_index,
      total_length: control.total_length,
      item_commitment: control.item_commitment,
      address: control.output_scan.address,
      lovelace: control.output_scan.lovelace,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: control.output_scan.cardano_value_size,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

/// The ResolveInputs pending-proof claimed successor when its verifier and
/// its membership step semantics admit it.
fn ri_successor(
  descriptor_cbor: ByteArray,
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base_tx = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base_tx,
    body: MidgardTransactionBody { ..base_tx.body, inputs: [input] },
  }
  when lop.step_v1(before, witness) is {
    Some(lop.LedgerOutputProofAdvanced { control: after }) -> {
      let Pair(base, _) = resolve_inputs_step_fixture(
        tx,
        singleton_root(key, descriptor_cbor),
        1,
        vm_witness.initial_resolution_accumulator(),
        8,
      )
      let Pair(pre, pre_cbor) = ri_state(
        tx,
        base,
        key,
        descriptor_cbor,
        before,
        9,
      )
      let Pair(post, _) = ri_state(tx, pre, key, descriptor_cbor, after, 10)
      let transition = vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: pre_cbor,
        claimed_successor: post,
      }
      if and {
        vm_resolve_inputs.verify_resolve_inputs(
          pre,
          transition,
          vm_machine_types.LedgerOutputProofStepWitness { witness },
        ),
        vm_resolve_inputs.verify_resolve_inputs_membership_step_semantics_v1(
          pre,
          transition,
          witness,
        ),
      } {
        Some(post.work_root)
      } else {
        None
      }
    }
    _ -> None
  }
}

fn narrow_head(tag: Int) -> fn(lraw.Frame, Data) -> Option<narrow.StepResult> {
  if tag == head_scalar_tag {
    narrow_datum.head_scalar
  } else if tag == head_sequence_tag {
    narrow_datum.head_sequence
  } else {
    narrow_datum.head_large_constructor
  }
}

/// Distinct ScriptSources and ResolveInputs successors of one datum-walk
/// head pre-state.
fn datum_successor_counts(
  output: MidgardTxOutput,
  before: lop.LedgerOutputProofControlV1,
  tag: Int,
  bound: Int,
) -> (Int, Int) {
  let output_cbor = encode_midgard_tx_output(output)
  let witnesses = list.map(
    admitted_actions(tag, bound),
    fn(action) { datum_witness(before, output_cbor, action) },
  )
  (
    distinct_count(
      list.filter_map(
        witnesses,
        fn(witness) { ss_successor(output, before, witness, narrow_head(tag)) },
      ),
    ),
    distinct_count(
      list.filter_map(
        witnesses,
        fn(witness) { ri_successor(pinned_descriptor(before), before, witness) },
      ),
    ),
  )
}

/// `d8799f 01 ff`: the root sequence's close position.
test datum_head_sequence_admits_one_successor() {
  let output = datum_output(#"d8799f01ff")
  let output_cbor = encode_midgard_tx_output(output)
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  datum_successor_counts(
    output,
    datum_root(output_cbor, proof),
    head_sequence_tag,
    8,
  ) == (1, 1)
}

/// `d8799f 01 ff`: the child scalar after the honest open.
test datum_head_scalar_admits_one_successor() {
  let output = datum_output(#"d8799f01ff")
  let output_cbor = encode_midgard_tx_output(output)
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  let root = datum_root(output_cbor, proof)
  let opened = advanced(
    lop.step_v1(
      root,
      datum_witness(
        root,
        output_cbor,
        honest_action(head_sequence_tag, [builtin.i_data(1)]),
      ),
    ),
  )
  datum_successor_counts(
    output,
    with_head_window(opened, proof),
    head_scalar_tag,
    4,
  ) == (1, 1)
}

/// `d8 66 82 18c8 80`: the root large constructor.
test datum_head_large_constructor_admits_one_successor() {
  let output = datum_output(#"d8668218c880")
  let output_cbor = encode_midgard_tx_output(output)
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  datum_successor_counts(
    output,
    datum_root(output_cbor, proof),
    head_large_constructor_tag,
    6,
  ) == (1, 1)
}
