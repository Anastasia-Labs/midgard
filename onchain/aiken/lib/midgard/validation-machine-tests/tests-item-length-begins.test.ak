use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardTransaction,
  MidgardTransactionBody, MidgardTransactionWitnessSet, MidgardVersionedScript,
  PlutusV3Script, SpendRedeemer,
}
use midgard/ledger_output_proof_v1
use midgard/redeemer_item_proof_v1 as item
use midgard/script_proof_v1 as leaves
use midgard/script_sources_raw_frame_v1 as frame
use midgard/validation_machine/cek
use midgard/validation_machine/machine_types as m
use midgard/validation_machine/script_sources as sources
use midgard/validation_machine/witness as work
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, test_l1_slot_config,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  cek_context_native_control_fixture, cek_state_fixture,
  empty_cek_context_control_fixture, empty_v1_transaction, inline_field_door,
  script_discovery_step_fixture, script_sources_output_step_fixture,
  v1_program_envelope,
}
use midgard/validation_machine_tests/fixtures_script_sources.{
  canonical_output_commitment_output,
}
use midgard/validation_merkle_v1 as merkle
use midgard/validation_trace_v1 as vm_trace

fn redeemer() -> MidgardRedeemerWitness {
  MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
}

fn transaction() -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: PlutusV3Script,
          script_bytes: v1_program_envelope(),
        },
      ],
      redeemer_tx_wits: [redeemer()],
    },
  }
}

fn begin(delta: Int) -> m.ValidationAuxiliaryWitnessV1 {
  let bytes = encode_midgard_redeemer_witness(redeemer())
  m.RedeemerScanBeginWitness {
    item_index: 0,
    item_count: 1,
    total_length: bytearray.length(bytes) + delta,
    item_commitment: bounded_item_v1.from_bytes(8, 0, bytes),
    siblings: [],
  }
}

fn initial(delta: Int, purpose: Bool) -> item.RedeemerItemProofControlV1 {
  let bytes = encode_midgard_redeemer_witness(redeemer())
  item.initial_control_v1(
    item.mode_descriptor,
    0,
    1,
    bytearray.length(bytes) + delta,
    bounded_item_v1.from_bytes(8, 0, bytes),
    if purpose {
      0
    } else {
      -1
    },
    if purpose {
      0
    } else {
      -1
    },
  )
}

// The frontier always commits the honest length; both the begin witness AND
// its claimed successor carry the candidate length. Consequently every check
// except the length-bearing membership is satisfied by the forged candidate.
fn cek_begin(delta: Int) -> Bool {
  let tx = transaction()
  let leaf = leaves.redeemer_leaf_hash(0, redeemer())
  let frontier = merkle.build_frontier([leaf])
  let native = m.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: frontier.peaks,
  }
  let context = m.CekContextControlV1 {
    ..empty_cek_context_control_fixture(0),
    redeemer_leaf: leaf,
    purpose_kind: 0,
    purpose_index: 0,
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
    redeemer_context_control_hash: #"",
  }
  let next = m.CekContextControlV1 {
    ..context,
    redeemer_context_control_hash: item.hash_control_v1(initial(delta, True)),
  }
  let Pair(pre, cbor) = cek_state_fixture(
    tx,
    native,
    work.encode_cek_context_control_v1(context),
    0,
    40,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native,
    work.encode_cek_context_control_v1(next),
    0,
    41,
  )
  cek.verify_cek_context_step_semantics_v1(
    pre,
    m.ValidationOneStepWitnessV1 {
      work_witness_cbor: cbor,
      claimed_successor: post,
    },
    begin(delta),
    inline_field_door(),
    test_l1_slot_config,
  )
}

fn source_begin(stage: Int, delta: Int) -> Bool {
  let tx = transaction()
  expect [script] = tx.witness_set.script_tx_wits
  let source_leaf = leaves.source_leaf_hash(0, #"00", script)
  let source_frontier = merkle.build_frontier([source_leaf])
  let redeemer_frontier = merkle.build_frontier(
    [leaves.redeemer_leaf_hash(0, redeemer())],
  )
  let purpose_frontier = merkle.build_frontier(
    [
      leaves.purpose_leaf_hash(
        0,
        0,
        leaves.versioned_script_hash(script),
        #"0102",
      ),
    ],
  )
  let discovery = if stage == 10 {
    m.ScriptDiscoveryControlV1 {
      ..work.empty_script_discovery_control(),
      current_purpose_kind: 0,
      current_purpose_index: 0,
      current_script_hash: leaves.versioned_script_hash(script),
      current_subject: #"0102",
      source_cursor: 1,
      matched_source_index: 0,
      matched_language_tag: 3,
      matched_source_leaf: source_leaf,
      used_inline_bitmap: 1,
    }
  } else {
    m.ScriptDiscoveryControlV1 {
      ..work.empty_script_discovery_control(),
      purpose_cursor: 1,
      source_cursor: 1,
      used_inline_bitmap: 1,
      used_redeemer_bitmap: 1,
      execution_count: 1,
      execution_peaks: merkle.build_frontier(
        [
          leaves.execution_leaf_hash(
            3,
            leaves.purpose_leaf_hash(
              0,
              0,
              leaves.versioned_script_hash(script),
              #"0102",
            ),
            source_leaf,
            leaves.redeemer_leaf_hash(0, redeemer()),
          ),
        ],
      ).peaks,
    }
  }
  let next = m.ScriptDiscoveryControlV1 {
    ..discovery,
    redeemer_item_control_hash: item.hash_control_v1(initial(delta, False)),
  }
  let Pair(pre, cbor) = script_discovery_step_fixture(
    tx,
    stage,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    discovery,
    40,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    stage,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    next,
    41,
  )
  let transition = m.ValidationOneStepWitnessV1 {
    work_witness_cbor: cbor,
    claimed_successor: post,
  }
  if stage == 10 {
    sources.verify_script_sources_stage_ten_mismatch_semantics_v1(
      pre,
      transition,
      begin(delta),
    )
  } else {
    sources.verify_script_sources_stage_twelve_redeemer_semantics_v1(
      pre,
      transition,
      begin(delta),
    )
  }
}

fn output_begin(delta: Int) -> Bool {
  let base = empty_v1_transaction()
  let output = canonical_output_commitment_output()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let bytes = encode_midgard_tx_output(output)
  let commitment = bounded_item_v1.from_bytes(2, 0, bytes)
  let output_frontier = merkle.build_frontier(
    [leaves.output_leaf_hash(0, bytes)],
  )
  let empty = merkle.build_frontier([])
  let accumulator = work.initial_resolution_accumulator()
  let Pair(pre, cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    5,
    empty,
    empty,
    0,
    accumulator,
    work.empty_resolution_schedule_hash(),
    0,
    empty,
    0,
    output_frontier,
    40,
  )
  let total_length = bytearray.length(bytes) + delta
  let post_work = frame.append_extension_v1(
    cbor,
    ledger_output_proof_v1.encode_initial_control_v1(
      0,
      total_length,
      commitment,
    ),
  )
  let post = vm_trace.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: vm_trace.hash_work_witness(vm_trace.ScriptSources, 41, post_work),
  }
  sources.verify_script_sources_output_proof_begin_semantics_v1(
    pre,
    m.ValidationOneStepWitnessV1 {
      work_witness_cbor: cbor,
      claimed_successor: post,
    },
    0,
    total_length,
    commitment,
    [],
  )
}

test cek_stage_zero_accepts_honest_length() {
  cek_begin(0)
}

test cek_stage_zero_refuses_length_plus_one() {
  !cek_begin(1)
}

test cek_stage_zero_refuses_length_minus_one() {
  !cek_begin(-1)
}

test sources_stage_ten_accepts_honest_length() {
  source_begin(10, 0)
}

test sources_stage_ten_refuses_length_plus_one() {
  !source_begin(10, 1)
}

test sources_stage_ten_refuses_length_minus_one() {
  !source_begin(10, -1)
}

test sources_stage_twelve_accepts_honest_length() {
  source_begin(12, 0)
}

test sources_stage_twelve_refuses_length_plus_one() {
  !source_begin(12, 1)
}

test sources_stage_twelve_refuses_length_minus_one() {
  !source_begin(12, -1)
}

test output_stage_five_accepts_honest_length() {
  output_begin(0)
}

test output_stage_five_refuses_length_plus_one() {
  !output_begin(1)
}

test output_stage_five_refuses_length_minus_one() {
  !output_begin(-1)
}
