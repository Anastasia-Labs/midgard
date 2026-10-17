//// Q27 `min-ada`: bind a transaction output, from an accepted normal
//// transaction or a forced leaf, before adjudicating the exact compiled
//// minimum-Ada rule.

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}
use midgard/ledger_state
use midgard/transition_trace

pub type MinAdaFaultV1 {
  MinAdaTx { output_index: Int }
}

pub type ForcedSource {
  input_index: Int,
  output_index: Int,
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
}

pub type Args {
  forced_source: Option<ForcedSource>,
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  fault: MinAdaFaultV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
