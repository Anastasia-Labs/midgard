//// Goldens for the ledger-output-proof yield's rejection path. A refusing
//// stage yields a one-byte failure class, never a reason; each of the two
//// dispatchers that delegate to the yield maps (class, its own coordinates)
//// to one exact `RejectionReasonV1` and checks the rejected successor itself.
//// These pin all eight mappings byte for byte, refuse a marker outside the
//// four classes, and show that a rejected successor is admitted only under
//// the reason its own class and coordinates name.
////
//// The ResolveInputs mapping writes `input_index` as the interim -1: the
//// resolution schedule does not carry the input's field item ordinal yet.

use aiken/cbor
use fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1 as resolve_inputs
use fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1 as output_proof
use midgard/ledger_output_proof_yield
use midgard/rejection_reason_v1.{RejectionReasonV1}
use midgard/validation_machine/shared
use midgard/validation_trace_v1 as vt

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

fn class_of(marker: ByteArray) -> Int {
  expect Some(class) = ledger_output_proof_yield.rejection_class_of(marker)
  class
}

fn own_output_reason_cbor(marker: ByteArray, output_index: Int) -> ByteArray {
  cbor.serialise(
    output_proof.own_output_rejection(class_of(marker), output_index),
  )
}

fn resolved_reason_cbor(marker: ByteArray, source_kind: Int) -> ByteArray {
  cbor.serialise(
    resolve_inputs.resolved_output_rejection(class_of(marker), source_kind),
  )
}

/// Arms 25-28 (tags 1298-1301), naming the output under the cursor.
test own_output_yield_reasons_are_pinned() {
  and {
    own_output_reason_cbor(#"00", 2) == #"d905129f02ff",
    own_output_reason_cbor(#"01", 2) == #"d905139f02ff",
    own_output_reason_cbor(#"02", 2) == #"d905149f02ff",
    own_output_reason_cbor(#"03", 2) == #"d905159f02ff",
  }
}

/// Arms 19-22 (tags 1292-1295), naming the source field and the interim -1.
test resolved_output_yield_reasons_are_pinned() {
  and {
    resolved_reason_cbor(#"00", 0) == #"d9050c9f0020ff",
    resolved_reason_cbor(#"01", 0) == #"d9050d9f0020ff",
    resolved_reason_cbor(#"02", 1) == #"d9050e9f0120ff",
    resolved_reason_cbor(#"03", 1) == #"d9050f9f0120ff",
  }
}

/// A one-byte successor control is a class marker; a marker beyond the four
/// classes is refused rather than read as a successor.
test yield_refuses_a_marker_outside_the_four_classes() fail {
  class_of(#"04") >= 0
}

/// A longer successor control is an advance, not a rejection.
test yield_reads_a_successor_control_as_no_class() {
  ledger_output_proof_yield.rejection_class_of(#"8000") == None
}

fn pre_state() -> vt.ValidationMachineStateV1 {
  vt.ValidationMachineStateV1 {
    machine_version: 1,
    event_key_hash: h32,
    transaction_id: h32,
    transaction_commitment: h32,
    validation_context_hash: h32,
    source_kind: vt.Normal,
    prior_ledger_root: h32,
    phase: vt.ScriptSources,
    program_counter: 7,
    work_root: vt.hash_work_witness(vt.ScriptSources, 7, #"80"),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: vt.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: h32,
  }
}

fn rejected_post(reason: RejectionReasonV1) -> vt.ValidationMachineStateV1 {
  let pre = pre_state()
  vt.ValidationMachineStateV1 {
    ..pre,
    phase: vt.Terminal,
    program_counter: pre.program_counter + 1,
    verdict: vt.Rejected,
    rejection_code_hash: vt.hash_rejection_code(reason),
    work_root: vt.hash_work_witness(
      vt.Terminal,
      pre.program_counter + 1,
      shared.encode_terminal_rejection_witness(reason, pre.prior_ledger_root),
    ),
  }
}

fn own_output_admits(
  post: vt.ValidationMachineStateV1,
  marker: ByteArray,
  output_index: Int,
) -> Bool {
  shared.rejected_successor_is_exact(
    pre_state(),
    post,
    output_proof.own_output_rejection(class_of(marker), output_index),
  )
}

/// The successor the output-proof dispatcher writes for a malformed reference
/// script at output 2 is admitted only under that class at that output, and
/// not under the resolved-input arm of the same class.
test own_output_yield_successor_is_exact() {
  let post = rejected_post(
    rejection_reason_v1.OutputReferenceScriptMalformed { output_index: 2 },
  )
  and {
    own_output_admits(post, #"01", 2),
    !own_output_admits(post, #"00", 2),
    !own_output_admits(post, #"02", 2),
    !own_output_admits(post, #"01", 3),
    !shared.rejected_successor_is_exact(
      pre_state(),
      post,
      resolve_inputs.resolved_output_rejection(class_of(#"01"), 0),
    ),
  }
}

/// The successor the ResolveInputs dispatcher writes for a non-canonical
/// spent output is admitted only under that class and source field.
test resolved_output_yield_successor_is_exact() {
  let reason = resolve_inputs.resolved_output_rejection(class_of(#"00"), 1)
  let post = rejected_post(reason)
  let admits = fn(marker, source_kind) {
    shared.rejected_successor_is_exact(
      pre_state(),
      post,
      resolve_inputs.resolved_output_rejection(class_of(marker), source_kind),
    )
  }
  and {
    admits(#"00", 1),
    !admits(#"00", 0),
    !admits(#"03", 1),
  }
}
