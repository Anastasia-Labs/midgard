use midgard/computation_thread as ct
use midgard/fraud_proofs/network_id/step_01.{NetworkIdFaultV1}
use midgard/ledger_state
use midgard/transition_trace

pub type Args {
  input_index: Int,
  output_index: Int,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  direction: Int,
}

/// Step 01 hands the thread over carrying exactly `ForcedNetworkIdMismatch`;
/// the shared `continue` helper requires a populated state on every
/// non-terminal handoff, so the forced door authenticates that marker rather
/// than an empty state.
pub type Datum =
  ct.StepDatum<NetworkIdFaultV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
