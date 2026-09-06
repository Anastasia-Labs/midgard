//// ScriptSources output-proof finalize dispatcher. Descriptor exactness of
//// the terminal ledger-output-proof control runs in the four authenticated
//// descriptor yields (`ledger-output-descriptor-*-yield.ak`, conjunction ==
//// `descriptor_is_exact_v1`); this validator opens the stage-five
//// ScriptSources frame, pins the terminal control bytes the yields attest
//// against, requires all four yields in every branch, and performs the
//// network / protected-output-authorization arms and the successor splice
//// itself. See
//// docs/fault-proofs/size-plans/validation-trace-script-sources-output-proof-finalize-semantic-v1.md
//// §4.1-4.2 and §10.x.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_roles
use midgard/ledger_output_v1
use midgard/rejection_reason_v1.{
  reject_missing_required_witness, reject_network_id_mismatch,
}
use midgard/script_proof_v1
use midgard/script_sources_raw_frame_v1 as raw
use midgard/validation_machine/machine_types.{
  InputSignerAuthorized, InputSignerMissing, InputSignerProofMalformed,
  LedgerOutputProofFinalizeWitness, ReceivePurposeScanControlV1,
  SignerSetProofV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_middle_stages.{
  protected_output_authorization, receive_source_successor,
}
use midgard/validation_machine/shared.{
  decode_validation_context, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  decode_frontier_peaks, encode_receive_purpose_scan_control,
}
use midgard/validation_merkle_v1
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_semantic_yield_v1
use midgard/validation_trace_v1.{ScriptSources, ValidationMachineStateV1}

/// The stage-five ScriptSources frame carries the ledger-output-proof control
/// as its extension item.
const output_proof_stage: Int = 5

const output_cursor_index: Int = 20

const output_count_index: Int = 21

const receive_scan_index: Int = 24

const extension_index: Int = 30

const frame_item_count: Int = 31

pub type ActionV1 {
  VerifyOutputProofFinalize {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
    /// The terminal ledger-output-proof control, pinned to frame item 30 and
    /// attested by every descriptor yield.
    control_cbor: ByteArray,
    /// `DataSummaryV1` on the wire; pinned by the value-summary yield and
    /// consumed by the scan-facts yield.
    claimed_value_summary: Data,
    /// `Option<DataSummaryV1>` on the wire; pinned by the datum-summary yield
    /// and consumed by the scan-facts yield.
    claimed_datum_summary: Data,
    /// Reference-input indices of the four descriptor yields, in
    /// `descriptor_roles` order, exactly.
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
      ct.Continue(VerifyOutputProofFinalize {
        input_index,
        output_index,
        transition,
        descriptor_cbor,
        signer_proof,
        control_cbor,
        claimed_value_summary: _,
        claimed_datum_summary: _,
        yield_ref_input_indices,
      }) -> {
        let auxiliary = LedgerOutputProofFinalizeWitness {
          descriptor_cbor,
          signer_proof,
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          output_proof_finalize_dispatch(
            datum |> semantic_pre_state,
            transition,
            descriptor_cbor,
            signer_proof,
            control_cbor,
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

fn output_proof_finalize_dispatch(
  pre: ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
  control_cbor: ByteArray,
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
  let output_cursor = raw.item_int_v1(frame, output_cursor_index)
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  and {
    // The four descriptor yields together re-state `descriptor_is_exact_v1`
    // on exactly these control bytes and this descriptor; the monolith
    // checked it before either rejection arm, so they are required in every
    // branch.
    validation_semantic_yield_v1.require_semantic_yields_v1(
      tx,
      reference_script_auth_policy_id,
      ledger_output_proof_roles.descriptor_roles(),
      yield_ref_input_indices,
    ),
    raw.item_bytes_v1(frame, extension_index) == control_cbor,
    output_cursor < raw.item_int_v1(frame, output_count_index),
    // Bound locally so the descriptor leaf below is appended at the cursor
    // the yields authenticated the descriptor for.
    descriptor.output_index == output_cursor,
    {
      expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
        descriptor.address,
      )
      let context = decode_validation_context(frame.context_cbor)
      if address.network_id != context.expected_network_id {
        rejected_successor_is_exact(
          pre,
          transition.claimed_successor,
          reject_network_id_mismatch,
        )
      } else {
        when
          protected_output_authorization(
            address,
            raw.item_int_v1(frame, 6),
            raw.item_bytes_v1(frame, 7),
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
          InputSignerAuthorized -> {
            expect Some(scan_item) = list.at(frame.items, receive_scan_index)
            expect
                [
                  source_count_data, source_peaks_data, receive_count_data,
                  previous_hash_data, candidate_hash_data, descriptor_peaks_data,
                ]
              = builtin.un_list_data(scan_item)
            let scan = ReceivePurposeScanControlV1 {
              source_count: builtin.un_i_data(source_count_data),
              source_peaks: decode_frontier_peaks(source_peaks_data),
              receive_count: builtin.un_i_data(receive_count_data),
              previous_hash: builtin.un_b_data(previous_hash_data),
              candidate_hash: builtin.un_b_data(candidate_hash_data),
              descriptor_peaks: validation_merkle_v1.append_leaf(
                output_cursor,
                decode_frontier_peaks(descriptor_peaks_data),
                script_proof_v1.output_descriptor_leaf_hash(
                  output_cursor,
                  descriptor_cbor,
                ),
              ),
            }
            // Splice the higher-index item first: `replace_items_v1` targets
            // offsets of the pre frame, and the item-24 replacement leaves
            // item 20's offset intact while the reverse order would not.
            let scan_spliced = raw.replace_items_v1(
              frame,
              transition.work_witness_cbor,
              receive_scan_index,
              1,
              encode_receive_purpose_scan_control(
                receive_source_successor(scan, address),
              ),
            )
            let cursor_spliced = raw.replace_items_v1(
              frame,
              scan_spliced,
              output_cursor_index,
              1,
              cbor.serialise(output_cursor + 1),
            )
            raw.successor_is_exact_v1(
              pre,
              transition,
              raw.drop_extension_v1(cursor_spliced, control_cbor),
            )
          }
        }
      }
    },
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
