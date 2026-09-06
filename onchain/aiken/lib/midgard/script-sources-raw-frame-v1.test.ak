use aiken/builtin
use aiken/cbor
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/ledger_output_scan_v1
use midgard/script_sources_raw_frame_v1 as raw
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness
use midgard/validation_trace_v1 as validation_trace

const h = #"0000000000000000000000000000000000000000000000000000000000000000"

fn control(stage: Int, compact: ByteArray) -> ByteArray {
  witness.encode_script_sources_witness(
    compact,
    #"80",
    #"80",
    #"80",
    0,
    h,
    0,
    h,
    [],
    stage,
    0,
    [],
    0,
    [],
    0,
    h,
    h,
    0,
    0,
    [],
    0,
    0,
    [],
    0,
    witness.empty_receive_purpose_scan_control(),
    0,
    0,
    witness.empty_observer_purpose_scan_control(),
    witness.empty_mint_fold_control(),
    h,
  )
}

fn pre(compact: ByteArray) -> validation_trace.ValidationMachineStateV1 {
  validation_trace.ValidationMachineStateV1 {
    machine_version: 1,
    event_key_hash: h,
    transaction_id: h,
    transaction_commitment: native_tx_proof_commitment_v1(compact, #"80", #"80"),
    validation_context_hash: validation_trace.hash_validation_context(#"80"),
    source_kind: validation_trace.Forced,
    prior_ledger_root: h,
    phase: validation_trace.ScriptSources,
    program_counter: 0,
    work_root: validation_trace.hash_work_witness(
      validation_trace.ScriptSources,
      0,
      control(5, compact),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace.Pending,
    rejection_code_hash: h,
    ledger_delta_root: h,
  }
}

fn transition(compact: ByteArray) -> ValidationOneStepWitnessV1 {
  let p = pre(compact)
  ValidationOneStepWitnessV1 {
    work_witness_cbor: control(5, compact),
    claimed_successor: validation_trace.ValidationMachineStateV1 {
      ..p,
      program_counter: 1,
      work_root: validation_trace.hash_work_witness(
        validation_trace.ScriptSources,
        1,
        control(6, compact),
      ),
    },
  }
}

test stage_splice_equals_canonical_encoder(compact via fuzz.bytearray()) {
  let w = transition(compact)
  let frame = raw.open_frame_v1(pre(compact), w, 30, 5)
  and {
    raw.replace_stage_v1(frame, w.work_witness_cbor, 6) == control(6, compact),
    raw.successor_is_exact_v1(pre(compact), w, control(6, compact)),
  }
}

test extension_append_replace_and_drop_are_exact(
  args via fuzz.tuple(fuzz.bytearray(), fuzz.bytearray()),
) {
  let (first, next) = args
  let base = control(5, #"80")
  let extended = raw.append_extension_v1(base, first)
  let p = pre(#"80")
  let frame = raw.open_frame_v1(
    p,
    ValidationOneStepWitnessV1 {
      work_witness_cbor: extended,
      claimed_successor: p,
    },
    31,
    5,
  )
  and {
    raw.item_bytes_v1(frame, 30) == first,
    raw.drop_extension_v1(extended, first) == base,
    raw.replace_extension_v1(extended, first, next) == raw.append_extension_v1(
      base,
      next,
    ),
  }
}

test rejects_noncanonical_array_header() fail {
  let w = transition(#"80")
  let malformed = #"99001e"
    |> builtin.append_bytearray(
        builtin.slice_bytearray(2, 100000, w.work_witness_cbor),
      )
  expect _ = raw.open_frame_v1(
    pre(#"80"),
    ValidationOneStepWitnessV1 { ..w, work_witness_cbor: malformed },
    30,
    5,
  )
}

test rejects_wrong_stage() fail {
  expect _ = raw.open_frame_v1(pre(#"80"), transition(#"80"), 30, 6)
}

test rejects_pending_extension_in_plain_frame() fail {
  let w = transition(#"80")
  expect _ = raw.open_frame_v1(
    pre(#"80"),
    ValidationOneStepWitnessV1 {
      ..w,
      work_witness_cbor: raw.append_extension_v1(w.work_witness_cbor, #"80"),
    },
    30,
    5,
  )
}

test rejects_foreign_source_commitment() fail {
  expect _ = raw.open_frame_v1(pre(#"00"), transition(#"80"), 30, 5)
}

test rejects_splice_at_a_foreign_offset() fail {
  expect _ = raw.splice_v1(#"001122", 1, #"22", #"33")
}

test rejects_short_extension_suffix() fail {
  expect _ = raw.replace_extension_v1(#"981f00", #"0011223344", #"80")
}

test pinned_initial_subcontrols_equal_canonical_encoders() {
  and {
    raw.empty_observer_scan_cbor == witness.encode_observer_purpose_scan_control(
      witness.empty_observer_purpose_scan_control(),
    ),
    raw.empty_mint_fold_cbor == witness.encode_mint_fold_control(
      witness.empty_mint_fold_control(),
    ),
    raw.initial_output_scan_cbor == ledger_output_scan_v1.encode_control_v1(
      ledger_output_scan_v1.initial_control_v1(),
    ),
  }
}

test derived_field_offset_preserves_all_other_canonical_items(
  compact via fuzz.bytearray(),
) {
  let w = transition(compact)
  let frame = raw.open_frame_v1(pre(compact), w, 30, 5)
  and {
    raw.item_offset_v1(frame, 9) == frame.stage_offset,
    raw.item_offset_v1(frame, 30) == bytearray.length(w.work_witness_cbor),
    raw.replace_items_v1(frame, w.work_witness_cbor, 9, 1, cbor.serialise(6)) == control(
      6,
      compact,
    ),
  }
}

test derived_field_splice_rejects_an_out_of_range_field() fail {
  let w = transition(#"80")
  let frame = raw.open_frame_v1(pre(#"80"), w, 30, 5)
  expect _ = raw.replace_items_v1(frame, w.work_witness_cbor, 30, 1, #"00")
}
