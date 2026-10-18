use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/fraud_proofs/common.{verify_non_membership_carried}
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/input_existence_v1/types.{
  Absent, LedgerEvidenceV1, LedgerStateV1, OutOfDomain, Present,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/mpf_proof_v1
use midgard/rejection_reason_v1.{InputNotFound}

pub fn decisive_fault_v1(
  state: LedgerStateV1,
  evidence: LedgerEvidenceV1,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  expect substrate.subject_is_canonical_v1(state.subject)
  when Pair(state.selected_input, evidence) is {
    Pair(None, OutOfDomain) -> False
    Pair(Some(input), Present { value_hash, proof }) -> {
      expect
        mpf_proof_v1.has_value_hash(
          state.pre_utxos_root,
          encode_midgard_tx_input(input),
          value_hash,
          proof,
        )
      False
    }
    Pair(Some(input), Absent { non_membership }) -> {
      expect
        verify_non_membership_carried(
          non_membership,
          state.pre_utxos_root,
          encode_midgard_tx_input(input),
          reference_inputs,
          redeemers,
        )
      True
    }
    _ -> fail
  }
}

/// The observed input can refute its exact false absence claim, or prove a
/// fault earlier than a different claim. Claimed raw coordinates/outrefs were
/// reopened by select_step_v1; no merged cursor or redeemer key is substituted.
pub fn verdict_contradiction_v1(
  state: LedgerStateV1,
  evidence: LedgerEvidenceV1,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  let absent = decisive_fault_v1(state, evidence, reference_inputs, redeemers)
  if !absent {
    and {
      state.subject.direction == substrate.direction_wrongful_rejection,
      state.subject.rejection_reason == Some(
        InputNotFound {
          source_kind: state.source_kind,
          input_index: state.input_index,
        },
      ),
    }
  } else {
    expect Some(observed_input) = state.selected_input
    let observed = order.ResolutionPositionV1 {
      source_kind: state.source_kind,
      input_index: state.input_index,
      out_ref: encode_midgard_tx_input(observed_input),
      subcheck: order.input_absence_subcheck,
    }
    let claimed = when state.subject.rejection_reason is {
      None -> order.NotRequired
      Some(r) ->
        when order.resolution_coordinate_of_reason_v1(r) is {
          None -> order.NotRequired
          Some(Pair(source_kind, input_index)) ->
            when state.claimed_input is {
              // claimed_input_v1 reads no input only for a coordinate outside
              // the field it names (or outside both fields).
              None -> order.ResolutionOutOfDomain { source_kind, input_index }
              Some(claimed_input) ->
                order.ResolutionReopened(
                  order.ResolutionPositionV1 {
                    source_kind,
                    input_index,
                    out_ref: encode_midgard_tx_input(claimed_input),
                    subcheck: order.resolution_subcheck_of_reason_v1(r),
                  },
                )
            }
        }
    }
    !order.leaf_admits_fault_v1(
      state.subject,
      Some(state.network_order_context),
      order.ResolutionFault(observed),
      claimed,
    )
  }
}
