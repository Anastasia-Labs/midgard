use midgard/computation_thread as ct
use midgard/fraud_proofs/script_integrity_hash_missing/state.{BindStateV1}
use midgard/ledger_state
use midgard/transition_trace

pub type State =
  BindStateV1

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  header: ledger_state.HeaderV1,
  forced_membership: Option<
    transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  >,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
