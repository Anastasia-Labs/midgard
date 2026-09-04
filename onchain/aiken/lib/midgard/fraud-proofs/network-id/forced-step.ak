use midgard/computation_thread as ct
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

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
