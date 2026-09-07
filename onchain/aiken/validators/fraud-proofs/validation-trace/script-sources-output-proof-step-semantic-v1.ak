//// ScriptSources output-proof step dispatcher. The ledger-output-proof stage
//// transition runs in one authenticated stage yield of the shared LOP family
//// (`ledger-output-proof-*-yield.ak`); this validator opens the stage-five
//// ScriptSources frame, pins the pre control bytes the yield attests against,
//// requires the yield, and re-derives the successor by splicing the attested
//// successor control into the frame extension. See
//// docs/fault-proofs/size-plans/validation-trace-script-sources-output-proof-step-semantic-v1.md
//// §4.1-4.2 and §10.x.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/ledger_output_proof_raw
use midgard/ledger_output_proof_roles
use midgard/script_sources_raw_frame_v1 as raw
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ScriptSources, ValidationMachineStateV1}

/// The stage-five ScriptSources frame carries the ledger-output-proof control
/// as its extension item.
const output_proof_stage: Int = 5

const extension_index: Int = 30

const frame_item_count: Int = 31

pub type ActionV1 {
  VerifyOutputProofStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    /// `LedgerOutputProofWitnessV1` on the wire; decoded only by the yields.
    proof_witness: Data,
    /// The pre ledger-output-proof control, pinned to frame item 30.
    control_cbor: ByteArray,
    /// The successor control the stage yield attests, or `#""` in rejection
    /// mode.
    next_control_cbor: ByteArray,
    /// The decoded scalar sub-control claim a datum-family stage yield
    /// consumes, attested by the matching scalar yield; `constr 1 []` for
    /// every other role.
    claimed_scalar: Data,
    yield_role_index: Int,
    /// Reference-input indices of the required yields, stage yield first and
    /// then the attestation roles of `stage_attestation_roles`, exactly.
    yield_ref_input_indices: List<Int>,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(VerifyOutputProofStep {
        input_index,
        output_index,
        transition,
        proof_witness,
        control_cbor,
        next_control_cbor,
        claimed_scalar: _,
        yield_role_index,
        yield_ref_input_indices,
      }) ->
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          builtin.constr_data(32, [proof_witness]),
          output_proof_step_dispatch(
            datum |> semantic_pre_state,
            transition,
            control_cbor,
            next_control_cbor,
            yield_role_index,
            yield_ref_input_indices,
            reference_script_auth_policy_id,
            tx,
          ),
          own_out_ref,
          tx,
        )
    }
  }

  else(_) {
    fail
  }
}

fn output_proof_step_dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  control_cbor: ByteArray,
  next_control_cbor: ByteArray,
  yield_role_index: Int,
  yield_ref_input_indices: List<Int>,
  reference_script_auth_policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  let frame = raw.open_frame_v1(
    pre,
    transition,
    frame_item_count,
    output_proof_stage,
  )
  // Outer well-formedness of the agreed control bytes: version, stage range,
  // descriptor scalars and canonical re-encoding. `expect` so the checked
  // decode actually runs (an unused `let` is fully ignored in Aiken).
  expect _ = ledger_output_proof_raw.open(control_cbor)
  and {
    validation_semantic_yield_v1.require_semantic_yields_v1(
      tx,
      reference_script_auth_policy_id,
      [
        ledger_output_proof_roles.stage_role(yield_role_index),
        ..ledger_output_proof_roles.stage_attestation_roles(yield_role_index)
      ],
      yield_ref_input_indices,
    ),
    raw.item_bytes_v1(frame, extension_index) == control_cbor,
    if next_control_cbor == #"" {
      transition.claimed_successor.phase == validation_trace_v1.Terminal
    } else {
      raw.successor_is_exact_v1(
        pre,
        transition,
        raw.replace_extension_v1(
          transition.work_witness_cbor,
          control_cbor,
          next_control_cbor,
        ),
      )
    },
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
