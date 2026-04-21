use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{TxInclusionArgs}
use midgard/ledger_state.{MidgardInputsRoot, MidgardTxId}

pub type State {
  verified_tx1_id: MidgardTxId,
  verified_tx1_inputs_root: MidgardInputsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args =
  TxInclusionArgs

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
