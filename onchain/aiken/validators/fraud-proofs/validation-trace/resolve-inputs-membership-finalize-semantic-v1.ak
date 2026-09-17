//// ResolveInputs membership-finalize dispatcher. Descriptor exactness of the
//// pending terminal ledger-output-proof control runs in the four
//// authenticated descriptor yields (`ledger-output-descriptor-*-yield.ak`,
//// conjunction == `descriptor_is_exact_v1`), spread over the fact-attachment
//// steps: each attachment transaction carries one canonical group of
//// descriptor yields and records their fact commitments in the terminal
//// control; the thin terminal step carries no descriptor yields, recomputes
//// all four commitments from one redeemer triple, and performs the
//// input-signer-authorization arms and the accumulator successor itself. See
//// docs/fault-proofs/decisions/0004-checkpointed-ledger-output-facts.md
//// Decision section.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_raw
use midgard/ledger_output_proof_roles
use midgard/ledger_output_proof_stages as lop_stages
use midgard/ledger_output_v1
use midgard/rejection_reason_v1.{reject_missing_required_witness}
use midgard/resolve_inputs_control_v1 as carrier
use midgard/validation_machine/machine_types.{
  InputSignerAuthorized, InputSignerMissing, InputSignerProofMalformed,
  LedgerOutputProofFinalizeWitness, SignerSetProofV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  input_signer_authorization, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{resolved_input_accumulator_successor}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ResolveInputs, ValidationMachineStateV1}

pub type ActionV1 {
  VerifyMembershipFinalize {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
    /// The terminal ledger-output-proof control, pinned to
    /// `pending.output_proof_cbor` and attested by every descriptor yield.
    control_cbor: ByteArray,
    /// `DataSummaryV1` on the wire; pinned by the value-summary yield and
    /// consumed by the scan-facts yield.
    claimed_value_summary: Data,
    /// `Option<DataSummaryV1>` on the wire; pinned by the datum-summary yield
    /// and consumed by the scan-facts yield.
    claimed_datum_summary: Data,
    /// The descriptor roles whose facts this step attaches — exactly the
    /// first canonical `fact_attach_groups_v1` group with unrecorded facts —
    /// or `[]` for the thin terminal step, which requires every fact
    /// recorded and exact and carries no descriptor yields.
    attach_roles: List<Int>,
    /// Reference-input indices of this step's descriptor yields, in
    /// `attach_roles` order, exactly; `[]` at the thin terminal.
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
      ct.Continue(VerifyMembershipFinalize {
        input_index,
        output_index,
        transition,
        descriptor_cbor,
        signer_proof,
        control_cbor,
        claimed_value_summary,
        claimed_datum_summary,
        attach_roles,
        yield_ref_input_indices,
      }) -> {
        let auxiliary = LedgerOutputProofFinalizeWitness {
          descriptor_cbor,
          signer_proof,
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ResolveInputs,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          membership_finalize_dispatch(
            datum |> semantic_pre_state,
            transition,
            descriptor_cbor,
            signer_proof,
            control_cbor,
            claimed_value_summary,
            claimed_datum_summary,
            attach_roles,
            yield_ref_input_indices,
            reference_script_auth_policy_id,
            tx,
          ),
          own_out_ref,
          tx,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

fn membership_finalize_dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
  control_cbor: ByteArray,
  claimed_value_summary: Data,
  claimed_datum_summary: Data,
  attach_roles: List<Int>,
  yield_ref_input_indices: List<Int>,
  reference_script_auth_policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  let control = carrier.control_raw_from_witness(transition.work_witness_cbor)
  let pending = carrier.pending_raw_from_cbor(control.pending_cbor)
  let frame = ledger_output_proof_raw.open(control_cbor)
  if attach_roles != [] {
    // A fact-attachment step: the group's descriptor yields attest their
    // exactness clauses over exactly these control bytes, this descriptor and
    // these claimed summaries; the successor records the group's fact
    // commitments in the pending terminal control.
    and {
      validation_semantic_yield_v1.require_semantic_yields_v1(
        tx,
        reference_script_auth_policy_id,
        list.map(attach_roles, ledger_output_proof_roles.descriptor_role),
        yield_ref_input_indices,
      ),
      descriptor_cbor == pending.descriptor_cbor,
      control_cbor == pending.output_proof_cbor,
      carrier.control_raw_is_bound(pre, transition, control, pending),
      carrier.control_raw_lop_is_pinned(frame, pending.descriptor_cbor),
      when
        lop_stages.fact_attach(
          frame,
          attach_roles,
          descriptor_cbor,
          claimed_value_summary,
          claimed_datum_summary,
        )
      is {
        None -> False
        Some(next_frame) -> and {
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
                      output_proof_cbor: ledger_output_proof_raw.encode(
                        next_frame,
                      ),
                    },
                  ),
                },
              ),
            ),
          }
      },
    }
  } else {
    membership_terminal_dispatch(
      pre,
      transition,
      descriptor_cbor,
      signer_proof,
      control_cbor,
      claimed_value_summary,
      claimed_datum_summary,
      yield_ref_input_indices,
      control,
      pending,
      frame,
    )
  }
}

fn membership_terminal_dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
  control_cbor: ByteArray,
  claimed_value_summary: Data,
  claimed_datum_summary: Data,
  yield_ref_input_indices: List<Int>,
  control: carrier.ResolveInputsControlRawV1,
  pending: carrier.PendingRawV1,
  frame: ledger_output_proof_raw.Frame,
) -> Bool {
  and {
    // The thin terminal carries no descriptor yields: every fact commitment
    // recorded by the attachment steps must equal the commitment recomputed
    // from this redeemer's descriptor bytes and claimed summaries, which
    // together re-state `descriptor_is_exact_v1` on exactly these control
    // bytes and this descriptor; the monolith checked it before the rejection
    // arm, so the gate runs in every branch.
    yield_ref_input_indices == [],
    lop_stages.facts_are_exact(
      frame,
      descriptor_cbor,
      claimed_value_summary,
      claimed_datum_summary,
    ),
    descriptor_cbor == pending.descriptor_cbor,
    control_cbor == pending.output_proof_cbor,
    carrier.control_raw_is_bound(pre, transition, control, pending),
    carrier.control_raw_lop_is_pinned(frame, pending.descriptor_cbor),
    {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
        descriptor.address,
      )
      when
        input_signer_authorization(
          pending.source_kind,
          address,
          control.signer_count,
          control.signer_frontier_commitment,
          signer_proof,
        )
      is {
        InputSignerMissing ->
          rejected_successor_is_exact(
            pre,
            transition.claimed_successor,
            reject_missing_required_witness,
          )
        InputSignerProofMalformed -> False
        InputSignerAuthorized -> and {
            transition.claimed_successor.phase == ResolveInputs,
            transition.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
              ResolveInputs,
              pre.program_counter + 1,
              carrier.encode_control_raw(
                carrier.ResolveInputsControlRawV1 {
                  ..control,
                  cursor: control.cursor + 1,
                  accumulator: resolved_input_accumulator_successor(
                    control.accumulator,
                    pending.source_kind,
                    pending.key,
                    descriptor_cbor,
                  ),
                  remaining_schedule_hash: pending.next_schedule_hash,
                  pending_cbor: #"00",
                },
              ),
            ),
          }
      }
    },
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
