//// ResolveInputs membership-step dispatcher. The ledger-output-proof stage
//// transition itself runs in one authenticated stage yield of the shared LOP
//// family (`ledger-output-proof-*-yield.ak`); this validator binds the
//// resolve-inputs carrier, pins the pre control bytes the yield attests
//// against, requires the yield, and re-derives the successor by splicing the
//// attested successor control back into the pending record. See
//// docs/fault-proofs/size-plans/validation-trace-resolve-inputs-membership-step-semantic-v1.md
//// §4.2-4.4.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/ledger_output_proof_raw
use midgard/ledger_output_proof_roles
use midgard/resolve_inputs_control_v1 as carrier
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ResolveInputs, ValidationMachineStateV1}

pub type ActionV1 {
  VerifyMembershipStep {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    /// `LedgerOutputProofWitnessV1` on the wire; decoded only by the yield.
    proof_witness: Data,
    /// The pre ledger-output-proof control, pinned to `pending.output_proof_cbor`.
    control_cbor: ByteArray,
    /// The successor control the yield attests, or `#""` in rejection mode.
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
      ct.Continue(VerifyMembershipStep {
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
          ResolveInputs,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          builtin.constr_data(32, [proof_witness]),
          membership_step_dispatch(
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

fn membership_step_dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  control_cbor: ByteArray,
  next_control_cbor: ByteArray,
  yield_role_index: Int,
  yield_ref_input_indices: List<Int>,
  reference_script_auth_policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  let control = carrier.control_raw_from_witness(transition.work_witness_cbor)
  let pending = carrier.pending_raw_from_cbor(control.pending_cbor)
  let frame = ledger_output_proof_raw.open(control_cbor)
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
    control_cbor == pending.output_proof_cbor,
    carrier.control_raw_is_bound(pre, transition, control, pending),
    carrier.control_raw_lop_is_pinned(frame, pending.descriptor_cbor),
    successor_is_exact(pre, transition, control, pending, next_control_cbor),
  }
}

/// The mode lives in the channel: an empty successor control is a rejection,
/// whose exact rejected successor the yield has already checked against the
/// code it computed; any other value is an advance whose work root this
/// dispatcher re-derives from the authenticated pre bytes.
fn successor_is_exact(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  control: carrier.ResolveInputsControlRawV1,
  pending: carrier.PendingRawV1,
  next_control_cbor: ByteArray,
) -> Bool {
  if next_control_cbor == #"" {
    transition.claimed_successor.phase == validation_trace_v1.Terminal
  } else {
    and {
      transition.claimed_successor.phase == ResolveInputs,
      transition.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        ResolveInputs,
        pre.program_counter + 1,
        carrier.encode_control_raw(
          carrier.ResolveInputsControlRawV1 {
            ..control,
            pending_cbor: carrier.encode_pending(
              carrier.PendingRawV1 {
                ..pending,
                output_proof_cbor: next_control_cbor,
              },
            ),
          },
        ),
      ),
    }
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
