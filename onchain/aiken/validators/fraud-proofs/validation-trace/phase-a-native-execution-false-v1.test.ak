//// Transaction-level vectors for the purpose-keyed native execution
//// rejection. A native script evaluated for the execution with purpose key
//// (1, 2) ends false at its finalize step; the machine's one successor is the
//// rejected terminal `ExecutionNativeScriptFalse { purpose_kind: 1,
//// purpose_index: 2 }`. These drive the deployed phase-A native advance
//// dispatcher through `main.spend` over a full transaction, in both
//// polarities: a challenger carrying the exact successor wins against an
//// operator commitment that names another purpose, and a challenger carrying
//// any other reason loses against the honest commitment.

use cardano/transaction as l1
use fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1 as advance
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptFalse, RejectionReasonV1, WitnessNativeScriptFalse,
}
use midgard/validation_machine/machine_types as mt
use midgard/validation_machine_tests/fixtures_native_execution.{
  late_walk_keyed, reference,
}
use midgard/validation_machine_tests/fixtures_phase_a.{exact_rejection_post}
use midgard/validation_resolution_v1 as resolution
use midgard/validation_trace_v1 as vt

const purpose_kind = 1

const purpose_index = 2

const thread = "native-execution-false"

fn honest() -> RejectionReasonV1 {
  ExecutionNativeScriptFalse { purpose_kind, purpose_index }
}

/// Spends the dispute step whose operator committed the successor rejecting
/// with `operator_reason` while the challenger claims the one rejecting with
/// `challenger_reason`.
fn dispute(
  operator_reason: RejectionReasonV1,
  challenger_reason: RejectionReasonV1,
) -> Bool {
  let walk = late_walk_keyed(reference, purpose_kind, purpose_index)
  let pre = walk.false_finalize_pre
  let operator_post = exact_rejection_post(pre, operator_reason)
  let challenger_post = exact_rejection_post(pre, challenger_reason)
  let transition = mt.ValidationOneStepWitnessV1 {
    work_witness_cbor: walk.false_finalize_cbor,
    claimed_successor: challenger_post,
  }
  let transition_data: Data = transition
  let auxiliary: Data = mt.NoAuxiliaryWitness
  let state = resolution.PreparedValidationResolutionStateV1 {
    version: 1,
    resolution: resolution.ValidationResolutionStateV1 {
      version: 1,
      pre_state: pre,
      operator_successor_hash: vt.hash_machine_state(operator_post),
      challenger_successor_hash: vt.hash_machine_state(challenger_post),
    },
    evidence_hash: resolution.hash_one_step_evidence(transition_data, auxiliary),
  }
  let datum = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
  let datum_data: Data = datum
  let redeemer = ct.Continue(
    advance.VerifyAdvance { input_index: 0, output_index: 0, transition },
  )
  let raw_redeemer: Data = redeemer
  let input_template = fixture.ct_thread_input(thread)
  let input = l1.Input {
    ..input_template,
    output: l1.Output {
      ..input_template.output,
      datum: l1.InlineDatum(datum_data),
    },
  }
  let winning: Data = resolution.winning_resolution()
  let template = fixture.continue_mid_step_l1_tx_v1(thread, winning)
  let tx = l1.Transaction {
    ..template,
    inputs: [input],
    outputs: [fixture.next_step_output(thread, winning)],
    redeemers: [Pair(l1.Spend(fixture.own_out_ref()), raw_redeemer)],
  }
  advance.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(datum),
    redeemer,
    fixture.own_out_ref(),
    tx,
  )
}

/// A real fault: the operator's successor names purpose index 3; the
/// challenger's exact successor wins the step.
test native_false_dispute_awards_the_exact_purpose_key() {
  dispute(
    ExecutionNativeScriptFalse { purpose_kind, purpose_index: 3 },
    honest(),
  )
}

/// A real fault on the purpose kind wins the same way.
test native_false_dispute_awards_against_another_purpose_kind() {
  dispute(
    ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index },
    honest(),
  )
}

/// Against the honest commitment, a challenger naming another purpose index
/// loses.
test native_false_dispute_refuses_another_purpose_index() fail {
  dispute(
    honest(),
    ExecutionNativeScriptFalse { purpose_kind, purpose_index: 3 },
  )
}

/// Against the honest commitment, a challenger naming the witness-mode
/// sentinel key loses.
test native_false_dispute_refuses_the_witness_mode_key() fail {
  dispute(
    honest(),
    ExecutionNativeScriptFalse { purpose_kind: -1, purpose_index: -1 },
  )
}

/// Against the honest commitment, a challenger naming the witness-script arm
/// loses.
test native_false_dispute_refuses_the_witness_script_arm() fail {
  dispute(honest(), WitnessNativeScriptFalse { script_index: 0 })
}
