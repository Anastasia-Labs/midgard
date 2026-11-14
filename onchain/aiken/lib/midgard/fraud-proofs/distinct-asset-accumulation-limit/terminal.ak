//// Terminal decision for `distinctAssetAccumulationLimit` (arms 42, 43 and
//// 44, phase 12). The completed derivation names the first entry the
//// producer refuses, if any; its coordinate is the one the subject records
//// (`ValueFaultPositionV1`, inputs by schedule position).

use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{TerminalV1}
use midgard/fraud_proofs/first_fault_v1.{ValueFault}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type Datum =
  ct.StepDatum<TerminalV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// The fault decisive for the subject. For an acceptance it is a crossing.
/// For a rejection it is the recorded reason's justification: a recorded
/// accumulation-limit reason is justified only at the derived crossing; any
/// other reason is justified when there is no crossing, or when the reason
/// is at or before the crossing in the producer's order.
pub fn decisive_fault_holds_v1(state: TerminalV1) -> Bool {
  let subject = state.bound.subject
  when subject.rejection_reason is {
    None -> state.crossing != None
    Some(reason) ->
      when first_fault_v1.value_position_of_reason_v1(reason) is {
        Some(recorded) -> state.crossing == Some(recorded)
        None ->
          when state.crossing is {
            None -> True
            Some(crossing) ->
              first_fault_v1.leaf_admits_later_fault_v1(
                subject,
                state.bound.network_order_context,
                ValueFault(crossing),
              )
          }
      }
  }
}

/// A wrongful acceptance convicts on a crossing; a wrongful rejection
/// convicts when its recorded reason is not justified.
pub fn convicts_v1(state: TerminalV1) -> Bool {
  substrate.terminal_contradiction_v1(
    state.bound.subject,
    decisive_fault_holds_v1(state),
  )
}
