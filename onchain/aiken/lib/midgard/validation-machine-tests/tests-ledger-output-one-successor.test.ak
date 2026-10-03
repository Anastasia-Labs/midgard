//// One-successor vectors for the ledger-output-proof span attach and fact
//// attachment (architecture decision
//// `docs/fault-proofs/decisions/0004-checkpointed-ledger-output-facts.md`).
////
//// The award rule checks only the challenger's claimed successor, so every
//// step must admit exactly one successor from a given pre-state.
////
//// Span attach: the recorded window is derived from the pre control (it
//// starts at the span the consuming stage reads next and runs for
//// `min(chunk_bytes, total_length - start)` bytes), and the attach is
//// admitted only while the recorded window does not cover that span. The
//// vectors run the monolithic `step_v1`, the split role-23 yield
//// (`ledger_output_proof_span.attach` under `ledger_output_proof_yield.attest`)
//// and the monolithic ScriptSources and ResolveInputs step verifiers over the
//// same pre-state, and show that the derived window is the only successor:
//// the whole-output window a prover might aim for is refused everywhere, and
//// a covering window refuses a second attach.
////
//// Fact attachment: the monolithic ScriptSources stage-five verifier derives
//// every fact from the terminal control, so a successor recording facts over
//// other descriptor bytes is refused while the honest one wins. The
//// monolithic ResolveInputs attachment likewise ignores the pending
//// descriptor; a pending descriptor other than the derived one is refused
//// once, by the finalize's fact gate.

use aiken/builtin
use aiken/crypto
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
  MidgardTxOutput, MidgardValue, ScriptCredential,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_raw as lraw
use midgard/ledger_output_proof_span as span
use midgard/ledger_output_proof_v1 as lop
use midgard/ledger_output_proof_yield as shared
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/resolve_inputs as vm_resolve_inputs
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, validation_context_cbor,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  authenticated_output_material, empty_v1_transaction, inline_field_door,
  resolve_inputs_step_fixture, script_sources_active_output_proof_fixture,
  signer_frontier_for_tx, singleton_field_item_chunk_proof, singleton_root,
  submitted_source,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Outputs and the LOP driver
// ---------------------------------------------------------------------------

/// A script-locked output whose inline datum `d87980` is its last 3 bytes.
fn datum_output() -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: Some(#"d87980"),
    script_ref: None,
  }
}

/// An output carrying a three-byte Plutus V3 reference script.
const reference_script_output: ByteArray =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"

fn initial_of(output_cbor: ByteArray) -> lop.LedgerOutputProofControlV1 {
  lop.initial_control_v1(
    0,
    bytearray.length(output_cbor),
    bounded_item_v1.from_bytes(2, 0, output_cbor),
  )
}

fn advanced(
  result: Option<lop.LedgerOutputProofStepResultV1>,
) -> lop.LedgerOutputProofControlV1 {
  expect Some(lop.LedgerOutputProofAdvanced { control }) = result
  control
}

/// The span-attach witness: the chunk proofs are its only data.
fn attach_witness(
  proof: bounded_item_v1.ChunkProofV1,
) -> lop.LedgerOutputProofWitnessV1 {
  lop.LedgerOutputProofSpanAttach { chunk_proof: proof, next_chunk_proof: None }
}

/// The span-attach witness a prover aiming at the window
/// `[start, start + length)` can present. The witness carries no window, so
/// the aim changes nothing: this is the same witness as `attach_witness`.
fn aimed_attach_witness(
  proof: bounded_item_v1.ChunkProofV1,
  _start: Int,
  _length: Int,
) -> lop.LedgerOutputProofWitnessV1 {
  attach_witness(proof)
}

/// The recorded window's bytes, as a span-consuming step receives them.
fn recorded_window_bytes(
  control: lop.LedgerOutputProofControlV1,
  output_cbor: ByteArray,
) -> ByteArray {
  expect Some(window) = control.span_window
  bytearray.slice(output_cbor, window.start, window.start + window.length - 1)
}

/// The honest driver: span attach when admitted, else the witness-free step,
/// else the recorded window bytes, else the chunk proofs, until `target`.
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

/// `control` with the window `[start, start + length)` of `output_cbor`
/// recorded.
fn with_window(
  control: lop.LedgerOutputProofControlV1,
  output_cbor: ByteArray,
  start: Int,
  length: Int,
) -> lop.LedgerOutputProofControlV1 {
  lop.LedgerOutputProofControlV1 {
    ..control,
    span_window: Some(
      lop.LedgerOutputSpanWindowV1 {
        start,
        length,
        digest: crypto.blake2b_256(
          bytearray.slice(output_cbor, start, start + length - 1),
        ),
      },
    ),
  }
}

// ---------------------------------------------------------------------------
// The monolithic step and the split role-23 yield
// ---------------------------------------------------------------------------

fn monolithic_accepts(
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
  after: lop.LedgerOutputProofControlV1,
) -> Bool {
  lop.step_v1(before, witness) == Some(
    lop.LedgerOutputProofAdvanced { control: after },
  )
}

/// The role-23 yield exactly as deployed: the witness decoded from Data by
/// the yield's own decoder, the split attach over the pinned pre control, and
/// the shared attestation against the claimed successor control.
fn split_attach(
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
) {
  let witness_data: Data = witness
  span.attach(
    lraw.open(lop.encode_control_v1(before)),
    shared.span_attach_witness(witness_data),
  )
}

fn split_accepts(
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
  after: lop.LedgerOutputProofControlV1,
) -> Bool {
  let witness_data: Data = witness
  let unused: Data = 0
  shared.attest(
    shared.Claim {
      pre_program_counter: 0,
      pre_prior_ledger_root: #"",
      claimed_successor: unused,
      witness: witness_data,
      control: lraw.open(lop.encode_control_v1(before)),
      next_control_cbor: lop.encode_control_v1(after),
      claimed_scalar: unused,
    },
    split_attach(before, witness),
  )
}

// ---------------------------------------------------------------------------
// The monolithic ScriptSources and ResolveInputs step verifiers
// ---------------------------------------------------------------------------

fn tx_producing(output: MidgardTxOutput) -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
}

fn ss_transition(
  output: MidgardTxOutput,
  before: lop.LedgerOutputProofControlV1,
  after: lop.LedgerOutputProofControlV1,
) -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  vm_machine_types.ValidationOneStepWitnessV1,
> {
  let tx = tx_producing(output)
  let frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, encode_midgard_tx_output(output))],
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
  Pair(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  )
}

fn ss_accepts(
  output: MidgardTxOutput,
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
  after: lop.LedgerOutputProofControlV1,
) -> Bool {
  let Pair(pre, transition) = ss_transition(output, before, after)
  and {
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

/// The resolve-inputs membership transaction (one input) and its key.
fn ri_tx_and_key() -> (MidgardTransaction, ByteArray) {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let base_tx = empty_v1_transaction()
  (
    MidgardTransaction {
      ..base_tx,
      body: MidgardTransactionBody { ..base_tx.body, inputs: [input] },
    },
    encode_midgard_tx_input(input),
  )
}

/// The resolve-inputs pre-state whose pending proof carries `descriptor_cbor`
/// (MPF-pinned at begin) and `before`.
fn ri_pre(
  descriptor_cbor: ByteArray,
  before: lop.LedgerOutputProofControlV1,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let (tx, key) = ri_tx_and_key()
  let Pair(base, _) = resolve_inputs_step_fixture(
    tx,
    singleton_root(key, descriptor_cbor),
    1,
    vm_witness.initial_resolution_accumulator(),
    8,
  )
  ri_state(tx, base, key, descriptor_cbor, before, 9)
}

/// A resolve-inputs pending-proof step from `ri_pre` to the pending proof
/// `after`.
fn ri_transition(
  descriptor_cbor: ByteArray,
  before: lop.LedgerOutputProofControlV1,
  after: lop.LedgerOutputProofControlV1,
) -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  vm_machine_types.ValidationOneStepWitnessV1,
> {
  let (tx, key) = ri_tx_and_key()
  let Pair(pre, pre_cbor) = ri_pre(descriptor_cbor, before)
  let Pair(post, _) = ri_state(tx, pre, key, descriptor_cbor, after, 10)
  Pair(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: pre_cbor,
      claimed_successor: post,
    },
  )
}

/// A resolve-inputs pending-proof step over the MPF-pinned honest descriptor.
fn ri_accepts(
  descriptor_cbor: ByteArray,
  before: lop.LedgerOutputProofControlV1,
  witness: lop.LedgerOutputProofWitnessV1,
  after: lop.LedgerOutputProofControlV1,
) -> Bool {
  let Pair(pre, transition) = ri_transition(descriptor_cbor, before, after)
  and {
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
  }
}

/// The honest descriptor bytes of the datum output's terminal control.
fn datum_output_descriptor(output_cbor: ByteArray) -> ByteArray {
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  let windowed = drive_to(
    initial_of(output_cbor),
    output_cbor,
    proof,
    lop.stage_datum_traversal,
    30,
  )
    |> fn(ready) { advanced(lop.step_v1(ready, attach_witness(proof))) }
  let opened = advanced(
    lop.step_v1(
      windowed,
      lop.LedgerOutputProofDatum {
        action: cek_data_traverse_v1.HeadSequence,
        window: Some(recorded_window_bytes(windowed, output_cbor)),
      },
    ),
  )
  let authenticated = advanced(
    lop.step_v1(
      opened,
      lop.LedgerOutputProofDatum {
        action: cek_data_traverse_v1.FinalizeFrame {
          frame: cek_data_frame_v1.initial_small_constr_frame_v1(0, #""),
          parent: None,
        },
        window: None,
      },
    ),
  )
  let terminal = drive_to(
    authenticated,
    output_cbor,
    proof,
    lop.stage_terminal,
    10,
  )
  expect Some(descriptor) = lop.terminal_descriptor_v1(terminal)
  ledger_output_commitment_v1.encode(descriptor)
}

// ---------------------------------------------------------------------------
// Span attach
// ---------------------------------------------------------------------------

/// Datum traversal with no window recorded: the attach records exactly the
/// tail window the datum step reads next, and the whole-output window is
/// refused by the monolithic step, the split yield and both machine
/// verifiers from the same pre-state.
test span_attach_datum_admits_one_successor() {
  let output = datum_output()
  let output_cbor = encode_midgard_tx_output(output)
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  let total = bytearray.length(output_cbor)
  let pre = drive_to(
    initial_of(output_cbor),
    output_cbor,
    proof,
    lop.stage_datum_traversal,
    30,
  )
  let derived = with_window(pre, output_cbor, total - 3, 3)
  let whole = with_window(pre, output_cbor, 0, total)
  let aimed = aimed_attach_witness(proof, 0, total)
  let descriptor_cbor = datum_output_descriptor(output_cbor)
  and {
    pre.span_window == None,
    monolithic_accepts(pre, attach_witness(proof), derived),
    split_accepts(pre, attach_witness(proof), derived),
    ss_accepts(output, pre, attach_witness(proof), derived),
    ri_accepts(descriptor_cbor, pre, attach_witness(proof), derived),
    !monolithic_accepts(pre, aimed, whole),
    !split_accepts(pre, aimed, whole),
    !ss_accepts(output, pre, aimed, whole),
    !ri_accepts(descriptor_cbor, pre, aimed, whole),
  }
}

/// Reference-script commitment: the attach records the window starting at
/// the reference-script item, the consuming step reads exactly that window,
/// and the whole-output window is refused.
test span_attach_reference_script_admits_one_successor() {
  let output_cbor = reference_script_output
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  let total = bytearray.length(output_cbor)
  let pre = drive_to(
    initial_of(output_cbor),
    output_cbor,
    proof,
    lop.stage_reference_script_commitment,
    30,
  )
  let offset = pre.output_scan.reference_script_item_offset
  let derived = with_window(pre, output_cbor, offset, total - offset)
  let whole = with_window(pre, output_cbor, 0, total)
  let aimed = aimed_attach_witness(proof, 0, total)
  and {
    offset > 0,
    monolithic_accepts(pre, attach_witness(proof), derived),
    split_accepts(pre, attach_witness(proof), derived),
    lop.step_v1(
      derived,
      lop.LedgerOutputProofWindow {
        bytes: recorded_window_bytes(derived, output_cbor),
      },
    ) != None,
    !monolithic_accepts(pre, aimed, whole),
    !split_accepts(pre, aimed, whole),
  }
}

/// Script-hash stage with no window recorded (the pre control carries none;
/// its content span starts past the reference-script tag): the attach
/// records the window starting at that content span and the whole-output
/// window is refused.
test span_attach_script_hash_admits_one_successor() {
  let output_cbor = reference_script_output
  let proof = singleton_field_item_chunk_proof(2, output_cbor)
  let total = bytearray.length(output_cbor)
  let reached = drive_to(
    initial_of(output_cbor),
    output_cbor,
    proof,
    lop.stage_script_hash,
    40,
  )
  let pre = lop.LedgerOutputProofControlV1 { ..reached, span_window: None }
  expect Some(hash_control) = pre.script_hash
  let (start, _) = lop.script_hash_content_span_v1(
    pre.output_scan.reference_script_offset,
    hash_control.cursor,
    hash_control.total_length,
  )
  let derived = with_window(pre, output_cbor, start, total - start)
  let whole = with_window(pre, output_cbor, 0, total)
  let aimed = aimed_attach_witness(proof, 0, total)
  and {
    start > 0,
    monolithic_accepts(pre, attach_witness(proof), derived),
    split_accepts(pre, attach_witness(proof), derived),
    !monolithic_accepts(pre, aimed, whole),
    !split_accepts(pre, aimed, whole),
  }
}

/// Once the recorded window covers the demanded span, the consuming step is
/// the only successor: a second attach is refused by the monolithic step and
/// the split yield, at the datum and reference-script stages and at the
/// script-hash stage the reference-script window already covers.
test span_attach_refused_while_window_covers() {
  let datum_cbor = encode_midgard_tx_output(datum_output())
  let datum_proof = singleton_field_item_chunk_proof(2, datum_cbor)
  let datum_windowed = drive_to(
    initial_of(datum_cbor),
    datum_cbor,
    datum_proof,
    lop.stage_datum_traversal,
    30,
  )
    |> fn(ready) { advanced(lop.step_v1(ready, attach_witness(datum_proof))) }
  let reference_proof = singleton_field_item_chunk_proof(
    2,
    reference_script_output,
  )
  let reference_windowed = drive_to(
    initial_of(reference_script_output),
    reference_script_output,
    reference_proof,
    lop.stage_reference_script_commitment,
    30,
  )
    |> fn(ready) {
        advanced(lop.step_v1(ready, attach_witness(reference_proof)))
      }
  let hash_covered = drive_to(
    initial_of(reference_script_output),
    reference_script_output,
    reference_proof,
    lop.stage_script_hash,
    40,
  )
  and {
    lop.step_v1(
      datum_windowed,
      aimed_attach_witness(datum_proof, 0, bytearray.length(datum_cbor)),
    ) == None,
    split_attach(
      datum_windowed,
      aimed_attach_witness(datum_proof, 0, bytearray.length(datum_cbor)),
    ) == None,
    lop.step_v1(reference_windowed, attach_witness(reference_proof)) == None,
    split_attach(reference_windowed, attach_witness(reference_proof)) == None,
    hash_covered.span_window != None,
    lop.step_v1(hash_covered, attach_witness(reference_proof)) == None,
    split_attach(hash_covered, attach_witness(reference_proof)) == None,
  }
}

// ---------------------------------------------------------------------------
// Fact attachment and the thin terminal (monolithic ScriptSources stage five)
// ---------------------------------------------------------------------------

fn protected_script_output() -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

fn finalize_auxiliary() -> vm_machine_types.ValidationAuxiliaryWitnessV1 {
  vm_machine_types.LedgerOutputProofFinalizeWitness {
    signer_proof: vm_machine_types.NoSignerSetProof,
  }
}

/// `control` with the leaf-summary facts (roles 2 and 3) committed over
/// `descriptor_cbor` together with each claimed summary.
fn with_descriptor_bound_leaf_facts(
  control: lop.LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
) -> lop.LedgerOutputProofControlV1 {
  expect Some((value_data, datum_data)) = lop.terminal_claimed_summaries_v1(
    control,
  )
  let commit = fn(summary: Data) {
    crypto.blake2b_256(
      builtin.serialise_data(
        builtin.list_data([builtin.b_data(descriptor_cbor), summary]),
      ),
    )
  }
  lop.with_fact_v1(
    lop.with_fact_v1(control, 2, commit(datum_data)),
    3,
    commit(value_data),
  )
}

/// From the fact-free terminal, the monolithic verifier records the leaf
/// group only as derived from the control: the honest successor wins, and a
/// successor recording the group over other descriptor bytes is refused.
test fact_attach_leaf_group_admits_one_successor() {
  let output = protected_script_output()
  let material = authenticated_output_material(encode_midgard_tx_output(output))
  expect Some(honest_next) = lop.fact_attach_v1(material.terminal)
  let forged_next = with_descriptor_bound_leaf_facts(material.terminal, #"00")
  let Pair(pre, honest_t) = ss_transition(
    output,
    material.terminal,
    honest_next,
  )
  let Pair(_, forged_t) = ss_transition(output, material.terminal, forged_next)
  and {
    honest_next != forged_next,
    vm_script_sources.verify_script_sources(
      pre,
      honest_t,
      finalize_auxiliary(),
      inline_field_door(),
    ),
    !vm_script_sources.verify_script_sources(
      pre,
      forged_t,
      finalize_auxiliary(),
      inline_field_door(),
    ),
  }
}

// ---------------------------------------------------------------------------
// Fact attachment and finalize (monolithic ResolveInputs)
// ---------------------------------------------------------------------------

/// Whether the monolithic ResolveInputs verifier, with the finalize
/// auxiliary, accepts the transition `t` from `pre` (it routes to the fact
/// attachment until every fact is recorded, then to the finalize).
fn ri_finalize_aux_accepts(
  pre: validation_trace_v1.ValidationMachineStateV1,
  t: vm_machine_types.ValidationOneStepWitnessV1,
) -> Bool {
  vm_resolve_inputs.verify_resolve_inputs(pre, t, finalize_auxiliary())
}

/// The finalize from the facts-complete pending proof `facts_terminal` whose
/// pending descriptor is `descriptor_cbor`: the claimed successor resolves
/// the input to exactly that descriptor.
fn ri_finalize_accepts(
  descriptor_cbor: ByteArray,
  facts_terminal: lop.LedgerOutputProofControlV1,
) -> Bool {
  let (tx, key) = ri_tx_and_key()
  let Pair(pre, pre_cbor) = ri_pre(descriptor_cbor, facts_terminal)
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let post_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    2,
    vm_witness.resolved_input_accumulator_successor(
      vm_witness.initial_resolution_accumulator(),
      0,
      key,
      descriptor_cbor,
    ),
    vm_witness.empty_resolution_schedule_hash(),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    None,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let t = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: pre_cbor,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      program_counter: 10,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ResolveInputs,
        10,
        post_cbor,
      ),
    },
  }
  and {
    ri_finalize_aux_accepts(pre, t),
    vm_resolve_inputs.verify_resolve_inputs_membership_finalize_semantics_v1(
      pre,
      t,
      vm_machine_types.NoSignerSetProof,
    ),
  }
}

/// The ResolveInputs fact attachment derives every fact from the pending
/// control, so a pending descriptor other than the derived one changes no
/// attachment successor; the finalize's `facts_are_exact_v1` gate is the one
/// check that refuses it, as on the split dispatcher path.
test resolve_inputs_forged_pending_descriptor_dead_ends_at_finalize() {
  let output = protected_script_output()
  let material = authenticated_output_material(encode_midgard_tx_output(output))
  let descriptor = ledger_output_commitment_v1.decode(material.descriptor_cbor)
  let forged_cbor = ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      ..descriptor,
      lovelace: descriptor.lovelace + 1,
    },
  )
  expect Some(next) = lop.fact_attach_v1(material.terminal)
  let Pair(honest_pre, honest_t) = ri_transition(
    material.descriptor_cbor,
    material.terminal,
    next,
  )
  let Pair(forged_pre, forged_t) = ri_transition(
    forged_cbor,
    material.terminal,
    next,
  )
  and {
    ri_finalize_aux_accepts(honest_pre, honest_t),
    ri_finalize_aux_accepts(forged_pre, forged_t),
    ri_finalize_accepts(material.descriptor_cbor, material.facts_terminal),
    !ri_finalize_accepts(forged_cbor, material.facts_terminal),
  }
}
